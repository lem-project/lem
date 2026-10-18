;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-tx-side
  (:use :cl)
  (:export #:*side-transactions*
           #:side-transaction
           #:side-undo
           #:side-clear))

(in-package :yeditor-tx-side)

;; Global registry for side‑transaction undo thunks.
;; Mapping to plan Cap. 5.3 kind‑2 (workspace side‑transaction; owner semantics deferred to T4/T3).
(defvar *side-transactions* nil
  "Alist mapping transaction name to its undo thunk.")

;; Global mutex to protect registry mutations (coordinator ruling).
(defparameter *side-lock* (sb-thread:make-mutex))

(defun side-transaction (name thunk undo-thunk)
  "Run THUNK immediately and register UNDO-THUNK under NAME.
   If NAME already exists, warn, run the OLD undo thunk, then replace with NEW undo.
   Returns NIL.
   NAME is a designator (e.g., keyword or string) used as key.
   THUNK is a function of no arguments performing the side effect.
   UNDO-THUNK is a function of no arguments that reverses the effect."
  (declare (type (or string symbol) name)
           (type function thunk undo-thunk))
  (funcall thunk)
  (sb-thread:with-mutex (*side-lock*)
    (let ((existing (assoc name *side-transactions* :test #'equal)))
      (if existing
          (progn
            (warn "Side transaction ~A redefined - previous undo executed and replaced." name)
            (let ((old-undo (cdr existing)))
              (when (functionp old-undo) (funcall old-undo)))
            (setf (cdr existing) undo-thunk))
          (push (cons name undo-thunk) *side-transactions*))))
  nil)

(defun side-undo (name)
  "Execute the undo thunk associated with NAME and remove it from the registry.
   If NAME is not present, do nothing."
  (declare (type (or string symbol) name))
  (sb-thread:with-mutex (*side-lock*)
    (let ((pair (assoc name *side-transactions* :test #'equal)))
      (when pair
        (let ((undo (cdr pair)))
          (when (functionp undo) (funcall undo)))
        (setf *side-transactions* (remove pair *side-transactions* :test #'equal)))))
  nil)

(defun side-clear ()
  "Clear all registered side‑transaction undo thunks."
  (sb-thread:with-mutex (*side-lock*)
    (setf *side-transactions* nil))
  nil)
