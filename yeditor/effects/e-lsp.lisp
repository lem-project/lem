;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-lsp
  (:use :cl :lem :yeditor-tx)
  (:export :install-lsp-session))
(in-package :yeditor-effects/e-lsp)

(defun install-lsp-session (comp buffer)
  "Start the language server session for BUFFER as a transaction effect (kind 12,
   Cap. 5.3). Undo stops the server process and unregisters its workspace.
   The handle carries :workspace-of, a function returning the started workspace.
   Reused sessions are borrowed and are preserved on undo."
  (let* ((cell (list nil))
         (owned nil)
         (handle (install-effect comp :lsp-session
                                 (lambda ()
                                   (setf (values (car cell) owned)
                                     (lem-lsp-mode:start-lsp-session buffer)))
                                 :undo-thunk
                                 (lambda ()
                                   (when (car cell)
                                     (when owned (lem-lsp-mode:stop-lsp-session (car cell)))
                                     (setf (car cell) nil owned nil))))))
    (push (cons :buffer buffer) handle)
    (push (cons :workspace-of (lambda () (car cell))) handle)
    handle))
