;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/lsp
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-lsp
           :*lsp-sessions*
           :lsp-start-buffer
           :lsp-stop-all
           :lsp-session-for
           :lsp-modeline-segment
           :yeditor-lsp-start
           :yeditor-lsp-stop-all))
(in-package :yeditor-components/lsp)

(defvar *lsp-sessions* nil
  "Alist (language-id . effect-handle) of language server sessions started by le-lsp.")

(defun buffer-spec (buffer)
  (lem-lsp-mode/spec:get-language-spec (lem/language-mode:buffer-language-mode buffer)))

(defun handle-workspace (handle)
  (funcall (cdr (assoc :workspace-of handle))))

(defun lsp-session-for (buffer)
  "The live workspace le-lsp started for the language of BUFFER, or nil."
  (let* ((spec (buffer-spec buffer))
         (handle (and spec (cdr (assoc (lem-lsp-mode/spec:spec-language-id spec) *lsp-sessions*
                                       :test (function equal)))))
         (workspace (and handle (handle-workspace handle))))
    (and workspace (lem-lsp-mode:lsp-session-alive-p workspace) workspace)))

(defun lsp-start-buffer (buffer)
  "Start (or reuse) the session for the language of BUFFER in its own transaction.
   Returns the workspace. Signals lem-lsp-mode:not-found-program when the server
   is not installed; nothing is left running in that case."
  (let ((spec (buffer-spec buffer)))
    (unless spec
      (editor-error "No language server is configured for ~A" (buffer-name buffer)))
    (or (lsp-session-for buffer)
        (let ((language (lem-lsp-mode/spec:spec-language-id spec))
              (yeditor-tx:*tx-pending* nil)
              (yeditor-tx:*tx-committed* nil))
          (let ((handle (install-lsp-session (quote le-lsp) buffer)))
            (yeditor-tx:tx-commit)
            (setf *lsp-sessions*
                  (acons language handle
                         (remove language *lsp-sessions* :key (function car) :test (function equal))))
            (handle-workspace handle))))))

(defun lsp-stop-all ()
  "Stop every session le-lsp started (newest first)."
  (dolist (entry *lsp-sessions*)
    (funcall (cdr (assoc :inverse (cdr entry)))))
  (setf *lsp-sessions* nil))

(defun install-hint (condition)
  (format nil "~A" condition))

(defun auto-start (buffer)
  (when (buffer-spec buffer)
    (handler-case (lsp-start-buffer buffer)
      (lem-lsp-mode:not-found-program (c) (message "~A" (install-hint c)))
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
  (handler-case (progn (lsp-start-buffer (current-buffer))
                       (message "Language server started"))
    (lem-lsp-mode:not-found-program (c) (message "~A" (install-hint c)))))

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
              (install-hook comp (quote *find-file-hook*) (quote auto-start))
              (yeditor-tx:install-effect comp :ui-state
                                         (lambda () (modeline-add-status-list (quote lsp-modeline-segment)))
                                         :undo-thunk
                                         (lambda () (modeline-remove-status-list (quote lsp-modeline-segment))))
              (yeditor-tx:install-effect comp :lsp-session (lambda () nil)
                                         :undo-thunk (function lsp-stop-all)))))
