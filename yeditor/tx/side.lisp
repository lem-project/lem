(defpackage :yeditor-tx-side
  (:use :cl)
  (:export #:*side-transactions*
           #:side-transaction
           #:side-undo
           #:side-clear))

(in-package :yeditor-tx-side)

(defvar *side-transactions* nil
  "Alist mapping transaction name to its undo thunk.")

(defun side-transaction (name thunk undo-thunk)
  "Run THUNK immediately and register UNDO-THUNK under NAME.
   If NAME already exists, signal a warning and replace the stored undo.
   Returns NIL.
   NAME is a designator (e.g., keyword or string) used as key.
   THUNK is a function of no arguments performing the side effect.
   UNDO-THUNK is a function of no arguments that reverses the effect."
  (declare (type (or string symbol) name)
           (type function thunk undo-thunk))
  (funcall thunk)
  (let ((existing (assoc name *side-transactions* :test #'equal)))
    (if existing
        (progn
          (warn "Side transaction ~A redefined – previous undo replaced." name)
          (setf (cdr existing) undo-thunk))
        (push (cons name undo-thunk) *side-transactions*)))
  nil)

(defun side-undo (name)
  "Execute the undo thunk associated with NAME and remove it from the registry.
   If NAME is not present, do nothing."
  (declare (type (or string symbol) name))
  (let ((pair (assoc name *side-transactions* :test #'equal)))
    (when pair
      (let ((undo (cdr pair)))
        (when (functionp undo)
          (funcall undo)))
      (setf *side-transactions* (remove pair *side-transactions* :test #'equal)))))

(defun side-clear ()
  "Clear all registered side‑transaction undo thunks."
  (setf *side-transactions* nil)
  nil)
