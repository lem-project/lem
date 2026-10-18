(defpackage :yeditor-effects/e-command
  (:use :cl :lem :yeditor-tx)
  (:export :install-command))
(in-package :yeditor-effects/e-command)

;; A simple command class that wraps a closure, used for dynamically
;; registered commands.  Inherits primary-command so Lem's dispatch accepts it.
(defclass yeditor-dynamic-command (lem::primary-command)
  ((fn :initarg :fn :reader dynamic-command-fn)))

(defmethod lem:execute ((command yeditor-dynamic-command) argument)
  (funcall (dynamic-command-fn command)))

(defun install-command (comp name fn &key (description ""))
  "Register a dynamic Lem command NAME (string) as a transaction effect (kind 7, Cap. 5.3).
   FN is a thunk called on execution.  Undo removes the command or restores
   the previous definition.  NAME is a string (Lem command names are strings)."
  (let ((prev (lem::find-command name)))
    (let ((instance (make-instance 'yeditor-dynamic-command
                                    :name name
                                    :fn fn)))
      (declare (ignore description))
      (let ((handle (install-effect comp :command
                                     (lambda () (lem::add-command name instance))
                                     :undo-thunk
                                     (lambda ()
                                       (if prev
                                           (lem::add-command name prev)
                                           (lem::remove-command name))))))
        (push (cons :command-name name) handle)
        (push (cons :prev prev) handle)
        handle))))
