;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects/e-mode
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-mode))
(in-package :yeditor-effects/e-mode)

(defun install-mode (comp buf mode)
  "Enable MODE on BUF as a transaction effect.
   The undo thunk restores the buffer's previous major mode (kind 3, Cap. 5.3).
   MODE is a function (buf) or a mode symbol accepted by lem:change-buffer-mode."
  (let ((prev-mode (lem:buffer-major-mode buf)))
    (let ((handle (install-effect comp :mode
                                   (lambda ()
                                     (if (symbolp mode)
                                         (lem:change-buffer-mode buf mode)
                                         (funcall mode buf)))
                                   :undo-thunk (lambda ()
                                                 (when prev-mode
                                                   (if (symbolp prev-mode)
                                                       (lem:change-buffer-mode buf prev-mode)
                                                       (funcall prev-mode buf)))))))
      (push (cons :mode mode) handle)
      (push (cons :buffer buf) handle)
      handle)))
