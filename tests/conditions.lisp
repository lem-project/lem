(in-package :lem-tests)

(define-condition test-error (simple-error)
  ((description
    :initarg :description
    :reader test-error-description))
  (:report (lambda (condition stream)
             (write-line (test-error-description condition) stream))))
