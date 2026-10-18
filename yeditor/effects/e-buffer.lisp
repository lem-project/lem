;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

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
