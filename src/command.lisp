(in-package :lem-core)

(define-condition executing-command (signal-handler)
  ((command :initarg :command
            :initform (alexandria:required-argument :command)
            :reader executing-command-command)))

(define-condition after-executing-command (executing-command) ()
  (:report (lambda (c s)
             (format s "after executing the ~S command" (executing-command-command c)))))

(defvar *pre-command-hook* '())
(defvar *post-command-hook* '())

(defvar *this-command*)

(defun this-command ()
  *this-command*)

(defgeneric execute (mode command argument))

(defun call-command (this-command universal-argument)
  (let ((*this-command* (ensure-command this-command)))
    (unless *this-command*
      (editor-error "~A: command not found" this-command))
    (run-hooks *pre-command-hook*)
    (prog1 (execute (get-active-modes-class-instance (current-buffer))
                    *this-command*
                    universal-argument)
      (buffer-undo-boundary)
      (signal-subconditions 'after-executing-command :command *this-command*)
      (run-hooks *post-command-hook*))))
