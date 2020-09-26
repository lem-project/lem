(defpackage :lem-base
  (:use :cl :lem-base.string-width-utils)
  #+sbcl
  (:lock t)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base.string-width-utils)
                     :collect (make-symbol (string sym)))))
