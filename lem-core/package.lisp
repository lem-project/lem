(defpackage :lem
  (:use :cl :lem-base)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym)))))

(defpackage :lem-user
  (:use :cl :lem))
