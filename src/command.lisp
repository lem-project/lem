(in-package :lem-core)

(defvar *pre-command-hook* '()
  "Normal hook run before each command is executed.")
(defvar *post-command-hook* '()
  "Normal hook run after each command is executed.")

(defvar *this-command*
  "The command now being executed.")

(defvar *current-prefix-arg* nil
  "The raw prefix argument for the current command.")

(defun this-command ()
  "Return the command now being executed."
  *this-command*)

(defgeneric execute (mode command argument))

(defun call-command (this-command universal-argument)
  "Call first argument as the command, passing remaining arguments to it."
  (let ((*this-command* (ensure-command this-command))
        (*current-prefix-arg* universal-argument))
    (unless *this-command*
      (editor-error "~A: command not found" this-command))
    (run-hooks *pre-command-hook*)
    (prog1 (execute (get-active-modes-class-instance (current-buffer))
                    *this-command*
                    universal-argument)
      (buffer-undo-boundary)
      (run-hooks *post-command-hook*))))
