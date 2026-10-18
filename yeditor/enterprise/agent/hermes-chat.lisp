;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

;;;; le-hermes-chat: chat panel talking to the local `hermes` CLI.
;;;; Each message runs `hermes chat -q <text> --format stream-json` in the
;;;; editor's working directory (project root preferred), streaming events into
;;;; the chat buffer, and resumes the same session on follow-ups so Hermes keeps
;;;; its context. Tool calls ("Running: <cmd> ..." lines) are shown dimmed.

(defpackage :yeditor-enterprise/hermes-chat
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-enterprise/journal
                #:record-agent-event)
  (:import-from :alexandria
                #:appendf #:when-let)
  (:import-from :split-sequence
                #:split-sequence)
  (:export :le-hermes-chat
           :yeditor-hermes-chat-toggle
           :yeditor-hermes-chat-send
           :yeditor-hermes-chat-focus
           :yeditor-hermes-chat-quit
           :submit-external-intent
           :*hermes-chat-buffer-name*
           :*hermes-binary*))
(in-package :yeditor-enterprise/hermes-chat)

(defparameter *hermes-chat-buffer-name* "*yeditor-hermes*"
  "Name of the Hermes Agent chat panel buffer.")

(defparameter *hermes-binary* "hermes"
  "The Hermes Agent CLI binary.")

(defparameter *hermes-chat-session* nil
  "Hermes session id to resume on the next message, or nil.")

(defvar *hermes-chat-keymap* (make-keymap :description '*hermes-chat-keymap*
                                            :base *global-keymap*)
  "Keymap active in the Hermes chat buffer.")

(setf (documentation '*hermes-chat-keymap* 'variable)
      "Keymap in the Hermes chat panel. Return sends the line, C-j inserts a
newline for multiline prompts, C-c C-k closes the panel and C-c C-a jumps
back to the last editor buffer without closing the panel.")

(define-key *hermes-chat-keymap* "Return" 'yeditor-hermes-chat-send)
(define-key *hermes-chat-keymap* "C-j" 'newline)
(define-key *hermes-chat-keymap* "C-c C-k" 'yeditor-hermes-chat-quit)
(define-key *hermes-chat-keymap* "C-c C-a" 'yeditor-hermes-chat-focus-editor)

(defun hermes-chat-active-p ()
  (alexandria:when-let (win (frame-bottomside-window (current-frame)))
    (eq (window-buffer win) (get-buffer *hermes-chat-buffer-name*))))

(defun make-hermes-chat-buffer ()
  (let ((buffer (make-buffer *hermes-chat-buffer-name*)))
    (change-buffer-mode buffer 'hermes-chat-mode)
    buffer))

(defun ensure-hermes-chat-buffer ()
  "Create or reuse the Hermes chat buffer."
  (or (get-buffer *hermes-chat-buffer-name*)
      (make-hermes-chat-buffer)))

(define-major-mode hermes-chat-mode ()
    (:name "Hermes Chat"
     :keymap *hermes-chat-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) t))

(defun chat-insert (buffer text &key (attribute nil))
  (with-buffer-read-only buffer nil
    (let ((point (buffer-end-point buffer)))
      (insert-string point (format nil "~A~%" text) :attribute attribute))))

(defun chat-insert-running (buffer text)
  "Insert an activity line (tool call) with a 'dimmed' look."
  (chat-insert buffer (format nil "  | ~A" text)))

;;;; input line handling: the prompt is a fixed last line "> " the user types
;;;; before; send cuts from the prompt marker to end of buffer.

(defun chat-prompt-point (buffer)
  (buffer-value buffer 'prompt-marker))

(defun (setf chat-prompt-point) (point buffer)
  (setf (buffer-value buffer 'prompt-marker) point))

(defun setup-prompt (buffer)
  (let ((point (buffer-end-point buffer)))
    (insert-string point "> ")
    (with-point ((p point))
      (buffer-start p)
      (character-offset (buffer-end p) -2)
      (setf (chat-prompt-point buffer) p))))

(defun chat-input-text (buffer)
  "Return the typed input sitting after the '> ' prompt, or nil."
  (alexandria:when-let ((marker (chat-prompt-point buffer)))
    (string-trim '(#\Space #\Tab #\Newline)
                 (points-to-string marker (buffer-end-point buffer)))))

(defun chat-clear-input (buffer)
  (let ((marker (chat-prompt-point buffer)))
    (when marker
      (with-point ((s marker)
                   (e (buffer-end-point buffer)))
        (delete-between-points s e)))))

(defun chat-redraw-prompt (buffer)
  "Ensure a fresh prompt line at the end of the buffer."
  (with-buffer-read-only buffer nil
    (let ((end (buffer-end-point buffer)))
      (unless (or (start-line-p end) (buffer-start-p end))
        (insert-string end (format nil "")))
      (unless (start-line-p (buffer-end-point buffer))
        (insert-character (buffer-end-point buffer) #\newline))
      (setup-prompt buffer))))

(defun chat-start-line-p (point)
  (line-start point)
  t)

;;;; hermes process

(defun hermes-chat-directory ()
  "Run hermes in the project root when known, else the buffer dir."
  (or (ignore-errors (namestring (lem-core/commands/project:find-root (buffer-directory))))
      (uiop:getcwd)))

(defun parse-stream-json-line (line buffer)
  "Turn one stream-json line into chat output."
  (alexandria:when-let ((event (ignore-errors (yason:parse line))))
    (let ((type (gethash "type" event)))
      (cond ((string= type "system")
             (setf *hermes-chat-session* (gethash "session_id" event)))
            ((string= type "text")
             (chat-insert buffer (gethash "text" event) :attribute 'modeline-position))
            ((string= type "tool")
             (chat-insert-running buffer (or (gethash "summary" event)
                                             (gethash "name" event)
                                             "tool call")))
            ((string= type "result")
             (let ((exit (gethash "exit_code" event)))
               (unless (and exit (zerop exit))
                 (chat-insert buffer (format nil "(hermes exited with code ~A)" exit)))))))))

(defun run-hermes-query (buffer query)
  "Run one hermes chat turn synchronously, streaming output into BUFFER."
  (let ((dir (hermes-chat-directory))
        (args (list "chat" "-q" query "--oneshot" "-Q" "--format" "stream-json")))
    (when *hermes-chat-session*
      (setf args (append args (list "--resume" *hermes-chat-session*))))
    (uiop:with-current-directory (dir)
      (multiple-value-bind (out err code)
          (run-hermes-capture args)
        (declare (ignore code))
        (dolist (line (split-string-lines out))
          (parse-stream-json-line line buffer))
        (when (and err (plusp (length (string-trim '(#\Space #\Newline) err))))
          (chat-insert buffer (format nil "(stderr) ~A" (string-trim '(#\Newline) err))))))))

(defun split-string-lines (string)
  (and string
       (plusp (length string))
       (split-sequence #\newline string)))

(defun run-hermes-capture (args)
  "Run the hermes CLI capturing stdout/stderr; returns (values out err code)."
  (let ((out nil) (err nil))
    (let* ((process (uiop:launch-program (cons *hermes-binary* args)
                                         :output :stream
                                         :error-output :stream))
           (out-thread (bt2:make-thread (lambda () (setf out (uiop:slurp-stream-string (uiop:process-info-output process))))))
           (err-thread (bt2:make-thread (lambda () (setf err (uiop:slurp-stream-string (uiop:process-info-error-output process)))))))
      (let ((code (uiop:wait-process process)))
        (bt2:join-thread out-thread)
        (bt2:join-thread err-thread)
        (values out err code)))))

(defun run-hermes-query-async (buffer query)
  "Run hermes in a thread so the editor stays responsive."
  (bt2:make-thread
   (lambda ()
     (handler-case (run-hermes-query buffer query)
       (error (e)
         (send-event (lambda ()
                       (chat-insert buffer (format nil "[error] ~A" e))))))
     (send-event (lambda ()
                   (with-buffer-read-only buffer nil
                     (chat-redraw-prompt buffer))))
     (record-agent-event *hermes-chat-session* query
                         :plan (format nil "user asked: ~A" query)
                         :tool-calls nil
                         :observation "done"))
   :name "yeditor-hermes-chat"))

(defun submit-hermes-message (text)
  "Send TEXT to hermes and stream the response into the chat buffer."
  (let ((buffer (ensure-hermes-chat-buffer)))
    (chat-insert buffer (format nil "[you] ~A" text) :attribute 'modeline-name)
    (chat-insert buffer "...")
    (run-hermes-query-async buffer text)))

(defun submit-external-intent (prompt)
  "Director Mode fallback: no MCP worker is on the bus, so run the hermes CLI
directly with the intent phrased to use the editor's MCP tools, and show the
result in the chat panel."
  (let* ((buffer (ensure-hermes-chat-buffer))
         (full (format nil
                       "Operate the Yeditor editor (repo at the current directory) using its MCP tools (yeditor_*). Then summarize what you changed. Intent: ~A"
                       prompt)))
    (make-bottomside-window buffer :height 14)
    (unless (buffer-value buffer 'greeted)
      (setf (buffer-value buffer 'greeted) t))
    (chat-insert buffer (format nil "[director] ~A" prompt) :attribute 'modeline-name)
    (chat-insert buffer "...")
    (run-hermes-query-async buffer full)))

(define-command yeditor-hermes-chat-send () ()
  "Send the current input line in the Hermes chat buffer."
  (let ((buffer (current-buffer))
        (text (chat-input-text (current-buffer))))
    (when (plusp (length text))
      (chat-clear-input buffer)
      (chat-redraw-prompt buffer)
      (submit-hermes-message text))))

(defun focus-last-editor-buffer ()
  (alexandria:when-let (win (frame-bottomside-window (current-frame)))
    (let ((windows (window-list)))
      (let ((main (find-if (lambda (w)
                             (and (not (eq w win))
                                  (not (typep w 'lem-core:side-window))))
                           windows)))
        (when main
          (switch-to-window main))))))

(define-command yeditor-hermes-chat-focus () ()
  "Focus the Hermes chat panel without toggling it closed."
  (alexandria:when-let (win (frame-bottomside-window (current-frame)))
    (switch-to-window win)))

(define-command yeditor-hermes-chat-focus-editor () ()
  "Jump back to the main editor window, leaving the chat panel open."
  (focus-last-editor-buffer))

(define-command yeditor-hermes-chat-quit () ()
  "Close the Hermes chat panel."
  (ignore-errors (delete-bottomside-window))
  (setf (frame-bottomside-window (current-frame)) nil)
  (message "Hermes chat closed"))

(define-command yeditor-hermes-chat-toggle () ()
  "Toggle the Hermes Agent chat panel (C-c h)."
  (if (hermes-chat-active-p)
      (yeditor-hermes-chat-quit)
      (let ((buffer (ensure-hermes-chat-buffer)))
        (make-bottomside-window buffer :height 14)
        (switch-to-window (frame-bottomside-window (current-frame)))
        (unless (buffer-value buffer 'greeted)
          (setf (buffer-value buffer 'greeted) t)
          (chat-insert buffer "Hermes Agent chat. Type after '> ' and press Return to send. C-c C-k closes, C-c C-a returns to the editor.")
          (chat-redraw-prompt buffer)))))

(define-component le-hermes-chat
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (when (boundp 'yeditor-components/director-cockpit::*intent-dispatcher*)
               (setf yeditor-components/director-cockpit::*intent-dispatcher*
                     (function submit-external-intent)))
             (list (install-keybinding comp *global-keymap* "C-c h" (quote yeditor-hermes-chat-toggle)))))