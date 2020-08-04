(in-package :lem-tests)

(defvar *anonymous-name-counter* 0)
(defvar *tests* '())

(defmacro define-test (name &body body)
  `(progn
     (unless (member ',name *tests*)
       (setf *tests* (nconc *tests* (list ',name))))
     (defun ,name () ,@body)))

(defun test (value description)
  (unless value
    (cerror "skip" 'test-error :description description)))

(defun default-test-handler (e)
  (warn "~A" e)
  (invoke-restart 'continue))

(defun run-test (test-fn)
  (handler-bind ((test-error #'default-test-handler))
    (funcall test-fn)))

(defun run-all-tests ()
  (lem-lisp-syntax:indentation-update)
  (let ((success t))
    (handler-bind ((test-error (lambda (e)
                                 (setq success nil)
                                 (default-test-handler e))))
      (dolist (test *tests*)
        (funcall test)))
    success))

(defun clear-all-tests ()
  (dolist (test *tests*)
    (fmakunbound test))
  (setq *tests* '())
  (setq *anonymous-name-counter* 0))

(defun generate-anonymous-test-name (&optional prefix)
  (alexandria:symbolicate "ANONYMOUS-"
                          (or prefix "")
                          (princ-to-string (incf *anonymous-name-counter*))))
