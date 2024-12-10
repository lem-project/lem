(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem-vi-mode/core
        :lem-vi-mode/ex)
  (:import-from :lem-vi-mode/options
                :option-value)
  (:import-from :lem-vi-mode/leader
                :leader-key)
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
                :*command-keymap*
                :*insert-keymap*
                :*inner-text-objects-keymap*
                :*outer-text-objects-keymap*
                :*operator-keymap*)
  (:import-from :lem-vi-mode/visual
                :visual
                :*visual-keymap*)
  (:import-from :lem-vi-mode/window
                :adjust-window-scroll)
  (:import-from :lem/kbdmacro
                :*macro-running-p*)
  (:import-from :alexandria
                :appendf)
  (:export :vi-mode
           :define-state
           :define-motion
           :define-operator
           :define-text-object-command
           :*motion-keymap*
           :*normal-keymap*
           :*command-keymap*
           :*insert-keymap*
           :*visual-keymap*
           :*operator-keymap*
           :*ex-keymap*
           :*inner-text-objects-keymap*
           :*outer-text-objects-keymap*
           :normal
           :insert
           :visual
           :change-state
           :option-value
           :leader-key))
(in-package :lem-vi-mode)

(defmethod post-command-hook ((state normal))
  (when *enable-repeat-recording*
    (let ((command (this-command)))
      (when (and (typep command 'vi-command)
                 (eq (vi-command-repeat command) t))
        (setf *last-repeat-keys* (vi-this-command-keys)))))
  (adjust-window-scroll)
  (fall-within-line (current-point)))

(defmethod post-command-hook ((state insert))
  (let* ((command (this-command))
         (this-command-keys (vi-this-command-keys))
         (pending-keys (cdr this-command-keys)))
    
    ;; For the command `self-insert`, the `this-command-keys` is typically only 1 char. (pending-keys = nil)
    ;; If pending-keys is NOT nil, then these keys are used to disguish the keys between `self-insert` command and other commands. 
    ;; For other commands, we can simply ignore the pending-keys.
    ;; For self-insert command, we should also flusthese pending-keys.
    (when (and
           (typep command 'self-insert)
           pending-keys)
      (loop :for key :in pending-keys
            :until (named-key-sym-p (key-sym key))
            :do 
               (self-insert 1 (key-to-char key))))
    
    (when *enable-repeat-recording*
      (unless (or (and (typep command 'vi-command)
                       (eq (vi-command-repeat command) nil))
                  (eq (command-name (this-command)) 'vi-end-insert))
        (appendf *last-repeat-keys*
                 this-command-keys))))
  (adjust-window-scroll))

(defmethod post-command-hook :after ((state visual))
  (adjust-window-scroll))

(defmethod state-enabled-hook ((state insert))
  (when *enable-repeat-recording*
    (setf *last-repeat-keys* nil))
  (unless *macro-running-p*
    (buffer-undo-boundary)
    (buffer-disable-undo-boundary (lem:current-buffer))))

(defmethod state-disabled-hook ((state insert))
  (unless *macro-running-p*
    (buffer-enable-undo-boundary (lem:current-buffer))))
