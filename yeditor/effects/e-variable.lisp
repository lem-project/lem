;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-variable
  (:use :cl :yeditor-tx)
  (:export :install-variable))
(in-package :yeditor-effects/e-variable)

(defun install-variable (comp var value)
  "Set dynamic global VAR to VALUE as a transaction effect (kind 6, Cap. 5.3).
   Captures the previous value (or unbound state) on each activation.
   Undo restores the previous value; if VAR was unbound when the effect was
   installed, undo calls makunbound.
   Re-override from multiple components uses LIFO order naturally (each
   effect captures the value it found; the last undo restores each layer)."
  (let ((was-bound nil)
        (prev nil))
    (let ((handle (install-effect comp :dynamic-global
                                   (lambda ()
                                     (setf was-bound (boundp var)
                                           prev (when was-bound (symbol-value var)))
                                     (when was-bound
                                       (warn "e-variable: overriding ~A; undo in LIFO order." var))
                                     (setf (symbol-value var) value))
                                   :undo-thunk
                                   (lambda ()
                                     (if was-bound
                                         (setf (symbol-value var) prev)
                                         (makunbound var))))))
      (push (cons :var var) handle)
      handle)))
