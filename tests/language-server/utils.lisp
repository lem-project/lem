(defpackage :lem-tests/language-server/utils
  (:use :cl
        :lem-language-server)
  (:export :with-mock-server
           :lines))
(in-package :lem-tests/language-server/utils)

(defun call-with-mock-server (function)
  (let ((lem-language-server::*debug-on-error* t)
        (lem-language-server::*server*))
    (start-mock-server)
    (funcall function)))

(defmacro with-mock-server (() &body body)
  `(call-with-mock-server (lambda () ,@body)))

(defun lines (&rest strings)
  (format nil "~{~A~%~}" strings))
