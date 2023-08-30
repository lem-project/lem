(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem-vi-mode/core
        :lem-vi-mode/ex)
  (:import-from :lem-vi-mode/options
                :option-value)
  (:import-from :lem-vi-mode/commands
                :vi-open-below
                :vi-open-above)
  (:import-from :lem-vi-mode/commands/utils
                :fall-within-line
                :define-motion
                :define-operator
                :define-text-object-command)
  (:import-from :lem-vi-mode/states
                :normal
                :insert
                :*motion-keymap*
                :*normal-keymap*
                :*insert-keymap*
                :*inner-text-objects-keymap*
                :*outer-text-objects-keymap*
                :*operator-keymap*)
  (:import-from :lem-vi-mode/visual
                :*visual-keymap*)
  (:import-from :alexandria
                :appendf)
  (:export :vi-mode
           :define-state
           :define-motion
           :define-operator
           :define-text-object-command
           :*motion-keymap*
           :*normal-keymap*
           :*insert-keymap*
           :*visual-keymap*
           :*operator-keymap*
           :*ex-keymap*
           :*inner-text-objects-keymap*
           :*outer-text-objects-keymap*
           :normal
           :insert
           :change-state
           :option-value))
(in-package :lem-vi-mode)

(defmethod post-command-hook ((state normal))
  (when *enable-repeat-recording*
    (let ((command (this-command)))
      (when (and (typep command 'vi-command)
                 (eq (vi-command-repeat command) t))
        (setf *last-repeat-keys* (vi-this-command-keys)))))
  (fall-within-line (current-point)))

(defmethod post-command-hook ((state insert))
  (let ((command (this-command)))
    (when *enable-repeat-recording*
      (unless (or (and (typep command 'vi-command)
                       (eq (vi-command-repeat command) nil))
                  (eq (command-name (this-command)) 'vi-end-insert))
        (appendf *last-repeat-keys*
                 (vi-this-command-keys))))
    (when (and (member (command-name command)
                       '(self-insert
                         ;; XXX: lem:call-command always adds a undo boundary
                         ;;  Delete the last boundary after these commands executed.
                         vi-open-below
                         vi-open-above)
                       :test 'eq)
               (eq :separator (lem-base::last-edit-history (current-buffer))))
      (vector-pop (lem-base::buffer-edit-history (current-buffer))))))

(defmethod state-enabled-hook ((state insert))
  (when *enable-repeat-recording*
    (setf *last-repeat-keys* nil))
  (buffer-undo-boundary))

(defmethod state-disabled-hook ((state insert))
  (unless (eq :separator (lem-base::last-edit-history (current-buffer)))
    (buffer-undo-boundary)))
