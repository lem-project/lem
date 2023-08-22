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
        (map 'list (lambda (char) (lem:make-key :sym (string char)))
             (princ-to-string (universal-argument-of-this-command))))
   (this-command-keys)))

(defmethod post-command-hook ((state normal))
  (let ((command (this-command)))
    (when (and (typep command 'vi-command)
               (eq (vi-command-repeat command) t))
      (setf *last-repeat-keys* (vi-this-command-keys)))))
