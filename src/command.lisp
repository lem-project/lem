(in-package :lem)

(define-condition executing-command (signal-handler)
  ((command :initarg :command
            :initform (alexandria:required-argument :command)
            :reader executing-command-command)))

(define-condition before-executing-command (executing-command) ()
  (:report (lambda (c s)
             (format s "before executing the ~S command" (executing-command-command c)))))

(define-condition after-executing-command (executing-command) ()
  (:report (lambda (c s)
             (format s "after executing the ~S command" (executing-command-command c)))))

(defconstant +primary-command-class-name+ 'primary-command)

(defvar *this-command*)

(defun this-command ()
  *this-command*)

(defgeneric execute (mode command argument))

(defclass primary-command () ())

(defun register-command-class (symbol class-name)
  (setf (get symbol 'command-class) class-name))

(defun get-command-class (symbol)
  (get symbol 'command-class))

(defun get-command (symbol)
  (alexandria:when-let (class (get-command-class symbol))
    (make-instance class)))

(defun call-command (this-command universal-argument)
  (let ((*this-command* (get-command this-command)))
    (unless *this-command*
      (editor-error "~A: command not found" this-command))
    (signal-subconditions 'before-executing-command :command *this-command*)
    (prog1 (execute (get-active-modes-class-instance (current-buffer))
                    *this-command*
                    universal-argument)
      (buffer-undo-boundary)
      (signal-subconditions 'after-executing-command :command *this-command*))))
