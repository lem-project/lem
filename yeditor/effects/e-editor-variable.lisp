;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-editor-variable
  (:use :cl :lem :yeditor-tx)
  (:export :install-editor-variable))
(in-package :yeditor-effects/e-editor-variable)

(defun install-editor-variable (comp symbol value)
  "Set the global value of the Lem editor variable SYMBOL to VALUE as a transaction
   effect (kind 6, Cap. 5.3). The previous global value is read at commit time,
   so stacked overrides unwind in LIFO order. Editor-variable change hooks (e.g.
   frame-multiplexer) run on both apply and undo."
  (let ((prev nil))
    (let ((handle (install-effect comp :editor-variable
                                  (lambda ()
                                    (setf prev (variable-value symbol :global))
                                    (setf (variable-value symbol :global) value))
                                  :undo-thunk
                                  (lambda ()
                                    (setf (variable-value symbol :global) prev)))))
      (push (cons :variable symbol) handle)
      handle)))
