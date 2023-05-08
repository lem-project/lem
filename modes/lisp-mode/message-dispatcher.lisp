f(in-package :lem-lisp-mode)

(defvar *message-dispatcher* (make-hash-table :test 'eq))

(defun get-message-dispatcher (name)
  (gethash name *message-dispatcher*))

(defmacro define-message ((name &rest params) &body body)
  (alexandria:with-unique-names (message)
    (let ((fn-name (alexandria:symbolicate "$$MESSAGE-DISPATCHER-" name)))
      `(progn
         (defun ,fn-name (,message)
           (destructuring-bind (,@params) (rest ,message)
             ,@body))
         (setf (gethash ,name *message-dispatcher*)
               ',fn-name)))))
