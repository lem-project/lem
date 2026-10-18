;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-hook
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-hook))
(in-package :yeditor-effects/e-hook)

(defun install-hook (comp hook-var fn)
  "Add FN to the hook list stored in HOOK-VAR (a symbol) as a transaction effect.
   Undo removes FN. add-hook and remove-hook are macros over a place, so the
   place is (symbol-value HOOK-VAR)."
  (let ((handle (install-effect comp :hook
                                 (lambda () (add-hook (symbol-value hook-var) fn))
                                 :undo-thunk
                                 (lambda () (remove-hook (symbol-value hook-var) fn)))))
    (push (cons :hook hook-var) handle)
    (push (cons :fn fn) handle)
    handle))
