;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-fdefinition
  (:use :cl :yeditor-tx)
  (:export :install-fdefinition))
(in-package :yeditor-effects/e-fdefinition)

(defun install-fdefinition (comp symbol fn)
  "Override the fdefinition of SYMBOL with FN as a transaction effect
   (kind 13, Cap. 5.3).  Captures the previous fdefinition on every activation.
   Undo restores it; if SYMBOL had no prior definition, undo calls fmakunbound.
   Re-override from multiple components uses LIFO order naturally.
   Warning is emitted when SYMBOL is already fbound (same-session re-override)."
  (let ((was-fbound nil)
        (prev nil))
    (let ((handle (install-effect comp :fdefinition-override
                                   (lambda ()
                                     (setf was-fbound (fboundp symbol)
                                           prev (when was-fbound (fdefinition symbol)))
                                     (when was-fbound
                                       (warn "e-fdefinition: overriding ~A; undo in LIFO order." symbol))
                                     (setf (fdefinition symbol) fn))
                                   :undo-thunk
                                   (lambda ()
                                     (if was-fbound
                                         (setf (fdefinition symbol) prev)
                                         (fmakunbound symbol))))))
      (push (cons :symbol symbol) handle)
      handle)))
