;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/lsp
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/palette #:*palette-actions*)
  (:export :le-lsp
           :*lsp-sessions*
           :lsp-start-buffer
           :lsp-stop-buffer
           :lsp-stop-all
           :lsp-session-for
           :lsp-modeline-segment
           :yeditor-lsp-start
           :yeditor-lsp-stop
           :yeditor-lsp-stop-all))
(in-package :yeditor-components/lsp)

(defvar *lsp-sessions* nil
  "Alist (session-key . effect-handles), with the newest buffer acquisition first.
   A session key is (language-id project-root-uri).")

(defun buffer-spec (buffer)
  (lem-lsp-mode/spec:get-language-spec (lem/language-mode:buffer-language-mode buffer)))

(defun handle-workspace (handle)
  (funcall (cdr (assoc :workspace-of handle))))

(defun lsp-session-for (buffer)
  "The live workspace le-lsp tracks for BUFFER's project and language, or NIL."
  (let ((workspace (lem-lsp-mode:buffer-lsp-session buffer)))
    (and workspace
         (some (lambda (entry)
                 (some (lambda (handle) (eq workspace (handle-workspace handle))) (cdr entry)))
               *lsp-sessions*)
         (lem-lsp-mode:lsp-session-alive-p workspace)
         workspace)))

(defun lsp-start-buffer (buffer)
  "Start (or reuse) BUFFER's project/language session in its own transaction.
   Requires active le-lsp; signals lem-lsp-mode:not-found-program if the server is missing."
  (unless (eq :active (getf (gethash 'le-lsp *component-registry*) :status))
    (editor-error "Language server support is disabled"))
  (let ((key (lem-lsp-mode:lsp-session-key buffer)))
    (unless key
      (editor-error "No language server is configured for ~A" (buffer-name buffer)))
    (let* ((entry (assoc key *lsp-sessions* :test #'equal))
           (workspace (and entry (handle-workspace (second entry)))))
      (when (and workspace (lem-lsp-mode:lsp-session-alive-p workspace)
                 (eq workspace (lem-lsp-mode:buffer-lsp-session buffer)))
        (return-from lsp-start-buffer workspace))
      (when (and entry (not (and workspace (lem-lsp-mode:lsp-session-alive-p workspace))))
        (undo-session entry)
        (setf *lsp-sessions* (remove entry *lsp-sessions* :test #'eq) entry nil))
      (let ((yeditor-tx:*tx-pending* nil)
            (yeditor-tx:*tx-committed* nil))
        (let ((new (install-lsp-session 'le-lsp buffer)))
          (yeditor-tx:tx-commit)
          (if entry
              (push new (cdr entry))
              (push (list key new) *lsp-sessions*))
          (handle-workspace new))))))

(defun undo-session (entry)
  (dolist (handle (cdr entry))
    (funcall (cdr (assoc :inverse handle)))))

(defun lsp-stop-buffer (buffer)
  "Stop le-lsp's session for BUFFER's project/language, preserving other projects."
  (let* ((key (lem-lsp-mode:lsp-session-key buffer))
         (entry (assoc key *lsp-sessions* :test #'equal)))
    (when entry
      (undo-session entry)
      (setf *lsp-sessions* (remove entry *lsp-sessions* :test #'eq)))))

(defun lsp-stop-all ()
  "Stop every session le-lsp started (newest first)."
  (dolist (entry *lsp-sessions*)
    (undo-session entry))
  (setf *lsp-sessions* nil))

(defun install-hint (condition)
  (format nil "~A" condition))

(defun offer-lsp-installation (condition &key (confirm #'prompt-for-y-or-n-p)
                                              (present #'show-message))
  (let ((hint (install-hint condition)))
    (when (funcall confirm (format nil "Install LSP? Show setup instructions.~%~A" hint))
      (funcall present hint))))

(defun auto-start (buffer)
  (when (buffer-spec buffer)
    (handler-case (lsp-start-buffer buffer)
      (lem-lsp-mode:not-found-program (c) (offer-lsp-installation c))
      (editor-error (c) (message "LSP: ~A" c)))))

(defun lsp-modeline-segment (window)
  (let* ((buffer (window-buffer window))
         (spec (buffer-spec buffer)))
    (cond ((null spec) "")
          ((lsp-session-for buffer)
           (format nil " LSP:~A " (lem-lsp-mode/spec:spec-language-id spec)))
          (t " LSP:off "))))

(define-command yeditor-lsp-start () ()
  "Start the language server for the current buffer, or explain how to install it."
  (install-component 'le-lsp)
  (handler-case (progn (lsp-start-buffer (current-buffer))
                       (message-without-log "Language server started"))
    (lem-lsp-mode:not-found-program (c) (offer-lsp-installation c))))

(define-command yeditor-lsp-stop () ()
  "Stop the language server for the current project and language."
  (lsp-stop-buffer (current-buffer))
  (message-without-log "Language server stopped for this project"))

(define-command yeditor-lsp-stop-all () ()
  "Stop every language server started by le-lsp."
  (lsp-stop-all)
  (message "Language servers stopped"))

(define-component le-lsp
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list
              ;; Lem would otherwise start servers from major-mode hooks, outside any transaction.
              (install-variable comp (quote lem-lsp-mode:*disable*) t)
              (install-variable comp '*palette-actions*
                                (append (list (cons "LSP: start for this project" #'yeditor-lsp-start)
                                              (cons "LSP: stop for this project" #'yeditor-lsp-stop))
                                        *palette-actions*))
              (install-hook comp (quote *find-file-hook*) (quote auto-start))
              (yeditor-tx:install-effect comp :ui-state
                                         (lambda () (modeline-add-status-list (quote lsp-modeline-segment)))
                                         :undo-thunk
                                         (lambda () (modeline-remove-status-list (quote lsp-modeline-segment))))
              (yeditor-tx:install-effect comp :lsp-session (lambda () nil)
                                         :undo-thunk (function lsp-stop-all)))))
