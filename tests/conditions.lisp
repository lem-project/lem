(defpackage :lem-tests/conditions
  (:use :cl)
  (:export :test-error
           :test-error-description))
(in-package :lem-tests/conditions)

(define-condition test-error (simple-error)
  ((description
    :initarg :description
    :reader test-error-description))
  (:report (lambda (condition stream)
             (write-string (test-error-description condition) stream))))
