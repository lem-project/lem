(in-package :lem)

(export '(*pre-command-hook*
          *post-command-hook*
          this-command
          primary-command))

(defconstant +primary-command-class-name+ 'primary-command)

(defvar *pre-command-hook* '())
(defvar *post-command-hook* '())

(defvar *this-command*)

(defun this-command ()
  *this-command*)

(defgeneric execute (command argument))

(defclass primary-command () ())

(defun get-command (symbol)
  (alexandria:when-let (class (find-class symbol nil))
    (make-instance class)))

(defun call-command (this-command universal-argument)
  (run-hooks *pre-command-hook*)
  (prog1 (alexandria:if-let (*this-command* (get-command this-command))
           (execute *this-command* universal-argument)
           (editor-error "~A: command not found" this-command))
    (buffer-undo-boundary)
    (run-hooks *post-command-hook*)))
