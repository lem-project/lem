;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise/hermes-chat
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/mcp
                #:worker-ids #:submit-task #:task-observations)
  (:import-from :yeditor-enterprise/journal
                #:record-agent-event)
  (:export :le-hermes-chat
           :yeditor-hermes-chat-toggle
           :yeditor-hermes-chat-send
           :*hermes-chat-buffer-name*))
(in-package :yeditor-enterprise/hermes-chat)

(defparameter *hermes-chat-buffer-name* "*yeditor-hermes*"
  "Name of the Hermes Agent chat panel buffer.")

(defparameter *hermes-chat-session* nil
  "The current chat session id, or nil.")

(defvar *hermes-chat-keymap* (make-keymap :description '*hermes-chat-keymap*
                                            :base *global-keymap*)
  "Keymap active in the Hermes chat buffer.")

(define-key *hermes-chat-keymap* "Return" 'yeditor-hermes-chat-send)
(define-key *hermes-chat-keymap* "C-c C-k" 'yeditor-hermes-chat-toggle)

(define-major-mode hermes-chat-mode ()
    (:name "Hermes Chat"
     :keymap *hermes-chat-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) t)
  (setf (buffer-read-only-p (current-buffer)) nil))

(defun ensure-hermes-chat-buffer ()
  "Create or reuse the Hermes chat buffer."
  (or (get-buffer *hermes-chat-buffer-name*)
      (let ((buffer (make-buffer *hermes-chat-buffer-name*)))
        (change-buffer-mode buffer 'hermes-chat-mode)
        buffer)))

(defun hermes-chat-active-p ()
  (and (frame-bottomside-window (current-frame))
       (eq (window-buffer (frame-bottomside-window (current-frame)))
           (get-buffer *hermes-chat-buffer-name*))))

(defun chat-insert (buffer text &key (attribute nil))
  (let ((point (buffer-end-point buffer)))
    (insert-string point (format nil "~A~%" text) :attribute attribute)))

(defun chat-input-text (buffer)
  "Extract the last line of BUFFER as user input."
  (with-point ((p (buffer-end-point buffer)))
    (line-start p)
    (string-trim '(#\Newline #\Space #\Tab)
                   (points-to-string p (buffer-end-point buffer)))))

(defun chat-clear-input (buffer)
  "Remove the last line after sending it."
  (with-point ((s (buffer-end-point buffer))
               (e (buffer-end-point buffer)))
    (line-start s)
    (delete-between-points s e)))

(defun worker-id-by-name (name)
  "Find the current MCP worker id named NAME, or nil."
  (find-if (lambda (id)
             (equal name (getf (gethash id yeditor-components/mcp::*workers*) :name)))
           (worker-ids)))

(defun wait-for-observation (task-id timeout-seconds)
  "Poll for an observation on TASK-ID; returns the observation plist or nil."
  (loop :for i :from 0 :below (* timeout-seconds 10)
        :do (let ((obs (assoc task-id (task-observations) :test #'equal)))
              (when obs (return obs)))
            (sleep 0.1))
  nil)

(defun display-hermes-response (buffer session task-id observation)
  (send-event
   (lambda ()
     (chat-insert buffer
                  (format nil "[hermes] ~A" (or observation "(no observation)"))
                  :attribute 'modeline-position)
     (record-agent-event session "chat"
                         :plan "chat response"
                         :tool-calls (list task-id)
                         :observation observation))))

(defun submit-hermes-message (text)
  "Send TEXT to the Hermes worker and record it in the journal."
  (let* ((session (or *hermes-chat-session*
                      (setf *hermes-chat-session* (format nil "chat-~D" (get-universal-time)))))
         (buffer (ensure-hermes-chat-buffer)))
    (record-agent-event session text
                        :plan (format nil "user asked: ~A" text)
                        :tool-calls nil
                        :observation "queued")
    (chat-insert buffer (format nil "[you] ~A" text) :attribute 'modeline-name)
    (bt2:make-thread
     (lambda ()
       (handler-case
           (let ((id (or (worker-id-by-name "hermes")
                         (progn (sleep 0.5) (worker-id-by-name "hermes")))))
             (if id
                 (let ((task-id (submit-task id "chat" text)))
                   (let ((obs (wait-for-observation task-id 120)))
                     (if obs
                         (display-hermes-response buffer session task-id (third obs))
                         (send-event (lambda ()
                                       (chat-insert buffer "[hermes] (timeout waiting for response)"))))))
                 (send-event (lambda ()
                               (chat-insert buffer "[hermes] worker not registered; install le-worker-hermes")))))
         (error (e)
          (send-event (lambda ()
                        (chat-insert buffer (format nil "[error] ~A" e)))))))
     :name "yeditor-hermes-chat")))

(define-command yeditor-hermes-chat-send () ()
  "Send the current input line in the Hermes chat buffer."
  (let ((buffer (ensure-hermes-chat-buffer)))
    (let ((text (chat-input-text buffer)))
      (chat-clear-input buffer)
      (unless (uiop:emptyp text)
        (submit-hermes-message text)))))

(define-command yeditor-hermes-chat-toggle () ()
  "Toggle the Hermes Agent chat panel (C-c h)."
  (if (hermes-chat-active-p)
      (progn (delete-bottomside-window)
             (message "Hermes chat hidden"))
      (let ((buffer (ensure-hermes-chat-buffer)))
        (make-bottomside-window buffer :height 14)
        (chat-insert buffer "Hermes Agent chat. Type a message and press Enter. C-c C-k closes the panel."))))

(define-component le-hermes-chat
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c h" (quote yeditor-hermes-chat-toggle)))))
