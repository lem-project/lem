(in-package :lem)

(defvar *executing-command*)

(defun executing-command ()
  *executing-command*)

(defgeneric execute (command argument))

(defclass command () ())

(defun get-command (symbol)
  (alexandria:when-let (class (find-class symbol nil))
    (make-instance class)))
