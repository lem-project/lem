(defpackage :lem-base/utils
  (:use :cl)
  (:export :pdebug))
(in-package :lem-base/utils)

(defun pdebug (x &optional (file "DEBUG"))
  (with-open-file (out file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (print x out)))
