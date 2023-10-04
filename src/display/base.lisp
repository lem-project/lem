(in-package :lem-core)

(defvar *inactive-window-background-color* nil)

(defun inactive-window-background-color ()
  *inactive-window-background-color*)

(defun (setf inactive-window-background-color) (color)
  (setf *inactive-window-background-color* color))

(defgeneric redraw-buffer (implementation buffer window force))

(defgeneric compute-left-display-area-content (mode buffer point)
  (:method (mode buffer point) nil))
