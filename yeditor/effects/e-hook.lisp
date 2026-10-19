;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-hook
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-hook :install-hook-removal))
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

(defun install-hook-removal (comp hook-var fn)
  "Remove FN from the hook list in HOOK-VAR as a transaction effect (kind 4).
   If FN was present at commit time, undo adds it back with its original weight."
  (let ((entry nil))
    (let ((handle (install-effect comp :hook
                                  (lambda ()
                                    (setf entry (find fn (symbol-value hook-var) :key (function car)))
                                    (when entry (remove-hook (symbol-value hook-var) fn)))
                                  :undo-thunk
                                  (lambda ()
                                    (when entry (add-hook (symbol-value hook-var) (car entry) (cdr entry)))))))
      (push (cons :hook hook-var) handle)
      handle)))
