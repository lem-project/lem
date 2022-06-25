(cl-lsp/defpackage:defpackage :cl-lsp/logger
  (:use :cl)
  (:import-from :cl-lsp/config
                :config)
  (:export :*enable-logger*
           :*logger-stream*
           :log-format
           :with-log-file
           :with-log-stream))
(in-package :cl-lsp/logger)

(defvar *logger-stream*)

(defun logger-enabled-p ()
  (config :enable-log))

(let ((lock (bt:make-lock)))
  (defun log-format (string &rest args)
    (bt:with-lock-held (lock)
      (when (boundp '*logger-stream*)
        (apply #'format *logger-stream* string args)
        (force-output *logger-stream*)))))

(defun call-with-log-file (file function)
  (if (logger-enabled-p)
      (let ((stream (open file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)))
        (setf *logger-stream* stream)
        (unwind-protect (funcall function)
          (close stream)))
      (funcall function)))

(defmacro with-log-file ((file) &body body)
  `(call-with-log-file ,file (lambda () ,@body)))

(defmacro with-log-stream ((stream) &body body)
  `(if (logger-enabled-p)
       (let ((*logger-stream* ,stream))
         ,@body)
       (progn ,@body)))
