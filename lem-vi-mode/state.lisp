(in-package :lem-vi-mode)

(defstruct vi-state
  name
  keymap
  function)

(defvar *current-state*)

(defun current-state ()
  *current-state*)

(defmacro define-vi-state (name (&key keymap) &body body)
  `(setf (get ',name 'state)
         (make-vi-state :name ',name :keymap ,keymap :function (lambda () ,@body))))

(defun change-state (name)
  (let ((state (get name 'state)))
    (assert (vi-state-p state))
    (setf *current-state* name)
    (setf (mode-keymap 'vi-mode) (vi-state-keymap state))
    (change-element-name (format nil "[~A]" name))
    (funcall (vi-state-function state))))

(defmacro with-state (state &body body)
  (alexandria:with-gensyms (old-state)
    `(let ((,old-state (current-state)))
       (change-state ,state)
       (unwind-protect (progn ,@body)
         (change-state ,old-state)))))
