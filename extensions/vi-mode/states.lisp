(defpackage :lem-vi-mode/states
  (:use :cl
        :lem
        :lem-vi-mode/core)
  (:import-from :lem-vi-mode/commands/utils
                :vi-command
                :vi-command-repeat)
  (:export :*last-repeat-keys*))
(in-package :lem-vi-mode/states)

(defvar *last-repeat-keys* nil)

(defun vi-this-command-keys ()
  (append
   (and (numberp (universal-argument-of-this-command))
        (coerce (princ-to-string (universal-argument-of-this-command)) 'list))
   (this-command-keys)))

(defmethod post-command-hook ((state normal))
  (let ((command (this-command)))
    (when (and (typep command 'vi-command)
               (eq (vi-command-repeat command) t))
      (setf *last-repeat-keys* (vi-this-command-keys)))))
