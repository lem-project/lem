(defpackage :lem-lisp-mode/swank-modules
  (:use :cl)
  (:export :swank-modules
           :require-swank-modules))
(in-package :lem-lisp-mode/swank-modules)

(defparameter *swank-modules*
  '(:swank-trace-dialog
    :swank-package-fu
    :swank-presentations
    :swank-fuzzy
    :swank-fancy-inspector
    :swank-c-p-c
    :swank-arglists
    :swank-repl))

(defun swank-modules ()
  *swank-modules*)

(defun require-swank-modules (&optional (modules (swank-modules)))
  (dolist (module modules)
    (require module (swank::module-filename module))))
