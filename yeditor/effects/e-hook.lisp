;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-hook
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-hook :install-hook-removal))
(in-package :yeditor-effects/e-hook)

(defun install-hook (comp hook-var fn)
  "Queue FN in HOOK-VAR; undo removes only the entry created by this effect.
A pre-existing callback, including its weight, is preserved across activations."
  (let ((owned-entry nil))
    (let ((handle
            (install-effect
             comp :hook
             (lambda ()
               (setf owned-entry nil)
               (unless (find fn (symbol-value hook-var) :key #'car)
                 (add-hook (symbol-value hook-var) fn)
                 (setf owned-entry (find fn (symbol-value hook-var) :key #'car))))
             :undo-thunk
             (lambda ()
               (when (and owned-entry (boundp hook-var)
                          (member owned-entry (symbol-value hook-var) :test #'eq))
                 (remove-hook (symbol-value hook-var) fn))
               (setf owned-entry nil)))))
      (push (cons :hook hook-var) handle)
      ;; :FN is reserved for the transaction installer used during resume.
      (push (cons :callback fn) handle)
      handle)))

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
