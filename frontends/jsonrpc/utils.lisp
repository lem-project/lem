(defpackage :lem-jsonrpc/utils
  (:use :cl)
  (:export :pdebug
           :hash
           :with-error-handler
           :json-equal
           :pretty-json))
(in-package :lem-jsonrpc/utils)

(defvar *log-filename* (merge-pathnames "logs/jsonrpc.log" (lem:lem-home)))

(defun pdebug (fmt &rest args)
  (when *log-filename*
    (ensure-directories-exist *log-filename*)
    (with-open-file (out *log-filename*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (apply #'format out fmt args)
      (terpri out))))

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

(defun json-equal (x y)
  (string= (with-output-to-string (out) (yason:encode x out))
           (with-output-to-string (out) (yason:encode y out))))

(defun pretty-json (value)
  (with-output-to-string (out)
    (yason:encode value
                  (yason:make-json-output-stream out))))
