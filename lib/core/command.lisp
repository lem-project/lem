(in-package :lem)

(defvar *executing-command*)

(defun executing-command ()
  *executing-command*)

(defgeneric execute (command argument))

(defclass command () ())

(defmethod execute ((command symbol) argument)
  (alexandria:if-let (class (find-class command nil))
    (let ((*executing-command* (make-instance class)))
      (execute *executing-command* argument))
    (editor-error "~A: command not found" command)))

(defun get-command (symbol)
  (alexandria:when-let (class (find-class symbol nil))
    (make-instance class)))
