;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects/e-lsp
  (:use :cl :lem :yeditor-tx)
  (:export :install-lsp-session))
(in-package :yeditor-effects/e-lsp)

(defun install-lsp-session (comp buffer spec)
  "Start an LSP workspace for BUFFER using SPEC as a transaction effect
   (kind 12, Cap. 5.3).  SPEC is a lem-lsp-mode workspace spec plist.
   Undo disposes the workspace (shutdown + kill LSP process).
   Requires lem-lsp-mode to be loaded."
  (let ((workspace nil))
    (let ((handle (install-effect comp :lsp-session
                                   (lambda ()
                                     (setf workspace
                                           (lem-lsp-mode::make-workspace
                                            :spec spec
                                            :buffer buffer))
                                     (lem-lsp-mode::add-workspace workspace))
                                   :undo-thunk
                                   (lambda ()
                                     (when workspace
                                       (handler-case
                                           (lem-lsp-mode::dispose-workspace workspace)
                                         (error () nil)))))))
      (push (cons :buffer buffer) handle)
      handle)))
