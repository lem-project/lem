(defpackage :lem-capi.util
  (:use :cl)
  (:export
   :with-error-handler
   :dbg))
(in-package :lem-capi.util)

(defvar *debug* nil)

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (lem:pdebug (format nil "~%******ERROR******:~%~A~%" c))
                               (lem:pdebug (with-output-to-string (out)
                                             (uiop:print-backtrace :stream out :condition c))))))
         (progn ,@body))
     (error ())))

(defun dbg (x)
  (when *debug*
    (lem:pdebug x)))
