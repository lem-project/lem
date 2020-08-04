(in-package :lem-tests)

(define-condition test-error (simple-error)
  ((description
    :initarg :description
    :reader test-error-description))
  (:report (lambda (condition stream)
             (write-string (test-error-description condition) stream))))
