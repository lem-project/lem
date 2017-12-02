(in-package :lem-vi-mode)

(defstruct vi-state
  name
  keymap
  function)

(defmacro define-vi-state (name (&key keymap) &body body)
  `(setf (get ',name 'state)
         (make-vi-state :name ',name :keymap ,keymap :function (lambda () ,@body))))

(defun change-state (name)
  (let ((state (get name 'state)))
    (assert (vi-state-p state))
    (setf (mode-keymap 'vi-mode) (vi-state-keymap state))
    (change-element-name (format nil "[~A]" name))
    (funcall (vi-state-function state))))
