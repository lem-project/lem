(in-package :lem-core)

(defgeneric handle-signal (condition)
  (:method (condition)
    nil))

(define-condition signal-handler () ())

(defun signal-subconditions (condition &rest initargs)
  (dolist (c (collect-subclasses condition :include-itself nil))
    (apply #'signal c initargs)))
