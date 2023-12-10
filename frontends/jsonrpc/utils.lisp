(defpackage :lem-jsonrpc/utils
  (:use :cl)
  (:export :pdebug
           :hash
           :with-error-handler))
(in-package :lem-jsonrpc/utils)

(defun pdebug (fmt &rest args)
  (with-open-file (out "~/lem-jsonrpc.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format out fmt args)
    (terpri out)))

(defun hash (&rest args)
  (alexandria:plist-hash-table args :test #'equal))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (pdebug "~A"
                                       (with-output-to-string (stream)
                                         (format stream "~A~%" c)
                                         (uiop:print-backtrace :stream stream
                                                               :condition c)
                                         (force-output stream))))))
         ,@body)
     (error ())))
