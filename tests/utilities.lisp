(in-package :lem-tests)

(defvar *tests* '())

(defmacro define-test (name &body body)
  `(progn
     (unless (member ',name *tests*)
       (setf *tests* (nconc *tests* (list ',name))))
     (defun ,name () ,@body)))

(defun test (value description)
  (unless value
    (cerror "skip" 'test-error :description description)))

(defun run-test (test-fn)
  (handler-bind ((test-error (lambda (e)
                               (format t "~&~A~%" e)
                               (invoke-restart 'continue))))
    (funcall test-fn)))

(defun run-all-tests ()
  (dolist (test *tests*)
    (run-test test)))
