(defpackage :lem/hover
  (:use :cl :lem)
  (:export :show-hover))
(in-package :lem/hover)

(defvar *hover-window* nil)

(defun clear-hover-window ()
  (delete-popup-message *hover-window*)
  (setf *hover-window* nil))

(define-condition clear-hover-window (before-executing-command)
  ((command :initarg :command
            :reader clear-hover-window-command)))

(defmethod handle-signal ((condition clear-hover-window))
  (when (and (not (eq (current-window) *hover-window*))
             (or (typep (clear-hover-window-command condition) 'movable-advice)
                 (typep (clear-hover-window-command condition) 'editable-advice)))
    (clear-hover-window)))

(defun show-hover (buffer-or-string)
  (when *hover-window*
    (clear-hover-window))
  (setf *hover-window*
        (display-popup-message buffer-or-string
                               :timeout nil
                               :style '(:gravity :cursor))))
