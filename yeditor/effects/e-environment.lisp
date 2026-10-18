;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-environment
  (:use :cl :yeditor-tx)
  (:export :install-environment-variable))
(in-package :yeditor-effects/e-environment)

(defun install-environment-variable (comp name value)
  "Set the process environment variable NAME to VALUE (a string) as a transaction
   effect (kind 6). Undo restores the value found at commit time, or unsets NAME if
   it was not set. Child processes started while it is set inherit it."
  (let ((previous nil))
    (let ((handle (install-effect comp :environment
                                  (lambda ()
                                    (setf previous (uiop:getenv name))
                                    (sb-posix:setenv name value 1))
                                  :undo-thunk
                                  (lambda ()
                                    (if previous
                                        (sb-posix:setenv name previous 1)
                                        (sb-posix:unsetenv name))))))
      (push (cons :variable name) handle)
      handle)))
