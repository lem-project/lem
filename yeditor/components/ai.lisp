;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/ai
  (:use :cl :lem :yeditor-context :yeditor-effects :yeditor-components/models)
  (:import-from :yeditor-components/palette #:*palette-actions*)
  (:export :le-models
           :le-ollama
           :*default-provider*
           :ai-modeline-segment
           :ai-explain
           :ai-rewrite-region
           :ai-document-region
           :strip-code-fences
           :yeditor-ai-explain
           :yeditor-ai-rewrite
           :yeditor-ai-document
           :yeditor-ai-chat
           :yeditor-ai-pull-model
           :yeditor-ai-toggle-cloud
           :yeditor-ai-undo-last))
(in-package :yeditor-components/ai)

(defvar *default-provider* nil "The provider used by the AI commands, or nil.")

(defun provider ()
  (or *default-provider* (editor-error "No AI provider configured (install le-ollama)")))

;;; Core operations (synchronous; the commands run them on a worker thread)

(defparameter *code-only*
  "Reply with only the resulting code: no explanations and no Markdown code fences.")

(defun strip-code-fences (text)
  "Remove a surrounding ``` fence that a model may add despite instructions."
  (let ((trimmed (string-trim '(#\Newline #\Space) text)))
    (if (and (uiop:string-prefix-p "```" trimmed) (uiop:string-suffix-p trimmed "```"))
        (let ((first-newline (position #\Newline trimmed)))
          (string-trim '(#\Newline) (subseq trimmed (if first-newline (1+ first-newline) 3)
                                            (- (length trimmed) 3))))
        text)))

(defun rewrite-prompt (instruction code)
  (format nil "~A~%~%Instruction: ~A~%~%Code:~%~A" *code-only* instruction code))

(defun document-prompt (code)
  (format nil "Write a short documentation comment for this code, using the comment syntax ~
               of its language. ~A~%~%~A" *code-only* code))

(defvar *last-ai-edit* nil "Side-transaction name of the most recent AI edit.")

(defun ai-explain (provider text &key on-chunk)
  (provider-generate provider (format nil "Explain what this code does, concisely:~%~%~A" text)
                     :on-chunk on-chunk))

(defun replace-region-reversibly (name start end new-text)
  "Replace START..END with NEW-TEXT as a side-transaction named NAME (kind 2)."
  (let* ((buffer (point-buffer start))
         (offset (position-at-point start))
         (old-text (points-to-string start end)))
    (flet ((swap (remove insert)
             (with-point ((s (buffer-start-point buffer)) (e (buffer-start-point buffer)))
               (move-to-position s offset)
               (move-to-position e (+ offset (length remove)))
               (delete-between-points s e)
               (insert-string s insert))))
      (yeditor-tx-side:side-transaction name
                                        (lambda () (swap old-text new-text))
                                        (lambda () (swap new-text old-text))))
    (setf *last-ai-edit* name)
    new-text))

(defun ai-rewrite-region (provider start end instruction &key on-chunk)
  "Rewrite START..END following INSTRUCTION; the edit is a reversible side-transaction
   named ai-rewrite:<buffer>. Returns the new text."
  (let* ((code (points-to-string start end))
         (new (strip-code-fences
               (provider-generate provider (rewrite-prompt instruction code) :on-chunk on-chunk))))
    (replace-region-reversibly (format nil "ai-rewrite:~A" (buffer-name (point-buffer start)))
                               start end new)))

(defun ai-document-region (provider start end &key on-chunk)
  "Ask for documentation of the code in START..END and insert it above START as a
   reversible side-transaction named ai-document:<buffer>. Returns the inserted text."
  (let* ((code (points-to-string start end))
         (doc (strip-code-fences
               (provider-generate provider (document-prompt code) :on-chunk on-chunk)))
         (text (format nil "~A~%" (string-right-trim '(#\Newline) doc))))
    (with-point ((p start))
      (line-start p)
      (replace-region-reversibly (format nil "ai-document:~A" (buffer-name (point-buffer start)))
                                 p p text))))

;;; Running on a worker thread

(defun output-buffer ()
  (or (get-buffer "*yeditor-ai*") (make-buffer "*yeditor-ai*")))

(defun stream-to-output (title)
  "Prepare the output buffer and return an on-chunk function that appends to it from any thread."
  (let ((buffer (output-buffer)))
    (insert-string (buffer-end-point buffer) (format nil "~&--- ~A (~A) ---~%" title (provider-name (provider))))
    (pop-to-buffer buffer)
    (lambda (text)
      (send-event (lambda () (insert-string (buffer-end-point buffer) text) (redraw-display))))))

(defun run-in-background (function)
  (let ((provider (provider)))
    (bt2:make-thread
     (lambda ()
       (handler-case (funcall function provider)
         (error (e) (send-event (lambda () (message "AI: ~A" e))))))
     :name "yeditor-ai")))

(define-command yeditor-ai-explain (start end) (:region)
  "Explain the region; the answer streams into *yeditor-ai*."
  (let ((text (points-to-string start end))
        (on-chunk (stream-to-output "explain")))
    (run-in-background (lambda (p) (ai-explain p text :on-chunk on-chunk)))))

(define-command yeditor-ai-rewrite (start end instruction) (:region (:string "Rewrite instruction: "))
  "Rewrite the region following an instruction (reversible)."
  (let ((start (copy-point start :right-inserting)) (end (copy-point end :left-inserting)))
    (run-in-background
     (lambda (p)
       (let ((text (points-to-string start end)))
         (let ((new (strip-code-fences
                     (provider-generate p (rewrite-prompt instruction text)))))
           (send-event (lambda ()
                         (replace-region-reversibly (format nil "ai-rewrite:~A" (buffer-name (point-buffer start)))
                                                    start end new)
                         (message "Rewritten (M-x yeditor-ai-undo-last to revert)")))))))))

(define-command yeditor-ai-document (start end) (:region)
  "Insert a documentation comment above the region (reversible)."
  (let ((start (copy-point start :right-inserting)) (end (copy-point end :left-inserting)))
    (run-in-background
     (lambda (p)
       (let ((code (points-to-string start end)))
         (let ((doc (strip-code-fences
                     (provider-generate p (document-prompt code)))))
           (send-event (lambda ()
                         (with-point ((s start))
                           (line-start s)
                           (replace-region-reversibly
                            (format nil "ai-document:~A" (buffer-name (point-buffer start))) s s
                            (format nil "~A~%" (string-right-trim '(#\Newline) doc))))))))))))

(define-command yeditor-ai-chat (prompt) ((:string "Ask: "))
  "Ask the AI provider; the answer streams into *yeditor-ai*."
  (let ((on-chunk (stream-to-output "chat")))
    (run-in-background (lambda (p) (provider-generate p prompt :on-chunk on-chunk)))))

(define-command yeditor-ai-pull-model (model) ((:string "Model to pull: "))
  "Download a model into the local Ollama server."
  (let ((on-status (stream-to-output (format nil "pull ~A" model))))
    (run-in-background
     (lambda (p)
       (unless (typep p (quote ollama-provider)) (error "The current provider is not Ollama"))
       (ollama-pull p model :on-status (lambda (s) (funcall on-status (format nil "~A~%" s))))))))

(define-command yeditor-ai-undo-last () ()
  "Revert the most recent AI rewrite or documentation insertion."
  (if *last-ai-edit*
      (progn (yeditor-tx-side:side-undo *last-ai-edit*)
             (message "Reverted ~A" *last-ai-edit*)
             (setf *last-ai-edit* nil))
      (message "No AI edit to revert")))

(define-command yeditor-ai-toggle-cloud () ()
  "Allow or forbid providers that send code off this machine."
  (setf *allow-cloud* (not *allow-cloud*))
  (message "Cloud AI providers ~:[disabled~;ENABLED: code may leave this machine~]" *allow-cloud*))

;;; Modeline status (privacy banner)

(defun ai-modeline-segment (window)
  (declare (ignore window))
  (let ((p *default-provider*))
    (cond ((null p) "")
          ((provider-cloud-p p) (format nil " AI:~A(cloud~:[, off~;~]) " (provider-name p) *allow-cloud*))
          (t (format nil " AI:~A " (provider-name p))))))

;;; Components

(define-component le-models
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*palette-actions*
                                     (append
                                      (mapcar (lambda (entry)
                                                (cons (car entry)
                                                      (lambda () (call-command (cdr entry) nil))))
                                              '(("AI: chat" . yeditor-ai-chat)
                                                ("AI: explain selection" . yeditor-ai-explain)
                                                ("AI: rewrite selection" . yeditor-ai-rewrite)
                                                ("AI: document selection" . yeditor-ai-document)
                                                ("AI: download model" . yeditor-ai-pull-model)
                                                ("AI: toggle cloud access" . yeditor-ai-toggle-cloud)))
                                      *palette-actions*))
                   (yeditor-tx:install-effect comp :ui-state
                                              (lambda () (modeline-add-status-list (quote ai-modeline-segment)))
                                              :undo-thunk
                                              (lambda () (modeline-remove-status-list (quote ai-modeline-segment))))
                   (install-keybinding comp *global-keymap* "C-c a e" (quote yeditor-ai-explain))
                   (install-keybinding comp *global-keymap* "C-c a r" (quote yeditor-ai-rewrite))
                   (install-keybinding comp *global-keymap* "C-c a d" (quote yeditor-ai-document))
                   (install-keybinding comp *global-keymap* "C-c a c" (quote yeditor-ai-chat)))))

(define-component le-ollama
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (let ((ollama (make-instance (quote ollama-provider))))
               (list (install-variable comp (quote *default-provider*) ollama)
                     (yeditor-tx:install-effect comp :noop
                                                (lambda ()
                                                  (unless (provider-available-p ollama)
                                                    (message "Ollama is not running at ~A (local AI unavailable)"
                                                             (provider-base-url ollama))))
                                                :undo-thunk (lambda () nil))))))
