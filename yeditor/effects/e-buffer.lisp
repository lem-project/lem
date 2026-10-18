;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects/e-buffer
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-buffer))
(in-package :yeditor-effects/e-buffer)

(defun install-buffer (comp name)
  "Register a Lem buffer creation as a transaction effect.
   The buffer is created when the transaction commits; the undo thunk kills it."
  (let ((buf nil))
    (let ((handle (install-effect comp :buffer
                                   (lambda () (setf buf (lem:make-buffer name :enable-undo-p nil)))
                                   :undo-thunk (lambda () (when buf (lem:kill-buffer buf))))))
      (push (cons :name name) handle)
      handle)))
