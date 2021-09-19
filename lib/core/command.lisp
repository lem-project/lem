(in-package :lem)

(defclass command () ())
(defgeneric execute (command argument))

(defmethod execute ((command symbol) argument)
  (alexandria:if-let (class (find-class command nil))
    (execute (make-instance class) argument)
    (editor-error "~A: command not found" command)))

(defun get-command (symbol)
  (alexandria:when-let (class (find-class symbol nil))
    (make-instance class)))
