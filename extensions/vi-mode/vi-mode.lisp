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

(define-command adjust-window-scroll () ()
  (let* ((window (lem:current-window))
         (window-height (lem-core::window-height-without-modeline window))
         (cursor-y (lem:window-cursor-y window))
         (window-scroll-offset (option-value "scrolloff"))
         (scroll-offset (min (floor (/ window-height 2)) window-scroll-offset)))
    (cond
      ((< cursor-y scroll-offset)
       (lem:window-scroll window (- cursor-y scroll-offset)))
      ((and (< (- window-height scroll-offset) cursor-y)
            (< (- window-height cursor-y)
               (- (lem:buffer-nlines (lem:current-buffer))
                  (lem:line-number-at-point (lem:current-point)))))
       (lem:window-scroll window (- cursor-y (- window-height scroll-offset)))))))

(defmethod post-command-hook ((state normal))
  (when *enable-repeat-recording*
    (let ((command (this-command)))
      (when (and (typep command 'vi-command)
                 (eq (vi-command-repeat command) t))
        (setf *last-repeat-keys* (vi-this-command-keys)))))
  (adjust-window-scroll)
  (fall-within-line (current-point)))

(defmethod post-command-hook ((state insert))
  (let ((command (this-command)))
    (when *enable-repeat-recording*
      (unless (or (and (typep command 'vi-command)
                       (eq (vi-command-repeat command) nil))
                  (eq (command-name (this-command)) 'vi-end-insert))
        (appendf *last-repeat-keys*
                 (vi-this-command-keys))))))

(defmethod state-enabled-hook ((state insert))
  (when *enable-repeat-recording*
    (setf *last-repeat-keys* nil))
  (buffer-undo-boundary)
  (buffer-disable-undo-boundary (lem:current-buffer)))

(defmethod state-disabled-hook ((state insert))
  (buffer-enable-undo-boundary (lem:current-buffer)))
