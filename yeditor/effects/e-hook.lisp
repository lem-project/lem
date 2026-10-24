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

(defun restore-removed-hook (hook-var entry following)
  (when (and entry (boundp hook-var)
             (not (find (car entry) (symbol-value hook-var) :key #'car)))
    (let* ((current (symbol-value hook-var))
           ;; Preserve the original position among callbacks with equal weight.
           (anchor (find-if (lambda (candidate)
                              (and (= (cdr candidate) (cdr entry))
                                   (member candidate current :test #'eq)))
                            following))
           (tail (if anchor (member anchor current :test #'eq)
                     (member-if (lambda (candidate) (< (cdr candidate) (cdr entry))) current))))
      (setf (symbol-value hook-var) (append (ldiff current tail) (list entry) tail)))))

(defun install-hook-removal (comp hook-var fn)
  "Remove FN while preserving its entry, weight and relative hook order on undo.
A later user registration of FN owns its weight and is left untouched."
  (let ((entry nil) (following nil))
    (let ((handle
            (install-effect
             comp :hook
             (lambda ()
               (setf entry (find fn (symbol-value hook-var) :key #'car)
                     following (copy-list (cdr (member entry (symbol-value hook-var) :test #'eq))))
               (when entry (remove-hook (symbol-value hook-var) fn)))
             :undo-thunk
             (lambda ()
               (restore-removed-hook hook-var entry following)
               (setf entry nil following nil)))))
      (push (cons :hook hook-var) handle)
      handle)))
