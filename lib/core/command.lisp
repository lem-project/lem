(in-package :lem)

(export '(primary-command))

(defconstant +primary-command-class-name+ 'primary-command)

(defvar *executing-command*)

(defun executing-command ()
  *executing-command*)

(defgeneric execute (command argument))

(defclass primary-command () ())

(defun get-command (symbol)
  (alexandria:when-let (class (find-class symbol nil))
    (make-instance class)))
