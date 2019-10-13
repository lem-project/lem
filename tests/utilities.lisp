(in-package :lem-tests)

(defmacro test (form description)
  `(unless ,form
     (cerror "skip" (make-condition 'test-error
                                    :description ,description))))

(defun run-test (test-fn)
  (handler-bind ((test-error (lambda (e)
                               (format t "~&~A~%" e)
                               (invoke-restart 'continue))))
    (funcall test-fn)))
