(defpackage :yeditor-effects/e-fdefinition
  (:use :cl :yeditor-tx)
  (:export :install-fdefinition))
(in-package :yeditor-effects/e-fdefinition)

(defun install-fdefinition (comp symbol fn)
  "Override the fdefinition of SYMBOL with FN as a transaction effect
   (kind 13, Cap. 5.3).  Captures the previous fdefinition before install.
   Undo restores it; if SYMBOL had no prior definition, undo calls fmakunbound.
   Re-override from multiple components uses LIFO order naturally.
   Warning is emitted when SYMBOL is already fbound (same-session re-override)."
  (let* ((was-fbound (fboundp symbol))
         (prev (when was-fbound (fdefinition symbol))))
    (when was-fbound
      (warn "e-fdefinition: re-overriding ~A — LIFO semantics apply." symbol))
    (let ((handle (install-effect comp :fdefinition-override
                                   (lambda () (setf (fdefinition symbol) fn))
                                   :undo-thunk
                                   (lambda ()
                                     (if was-fbound
                                         (setf (fdefinition symbol) prev)
                                         (fmakunbound symbol))))))
      (push (cons :symbol symbol) handle)
      handle)))
