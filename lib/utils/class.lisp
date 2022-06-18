(defpackage :lem-utils/class
  (:use :cl)
  (:import-from :closer-mop)
  (:export :ensure-class
           :collect-subclasses))
(in-package :lem-utils/class)

#+sbcl
(sb-ext:lock-package :lem-utils/class)

(defun ensure-class (class)
  (etypecase class
    (class class)
    (symbol (find-class class))))

(defun collect-subclasses (class &key (include-itself
                                       (alexandria:required-argument :include-itself)))
  (labels ((rec (class include-itself)
             (let ((subclasses
                     (loop :for subclass :in (closer-mop:class-direct-subclasses class)
                           :append (rec subclass t))))
               (if include-itself
                   (cons class subclasses)
                   subclasses))))
    (rec (ensure-class class) include-itself)))
