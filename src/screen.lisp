(in-package :lem-core)

(defstruct (screen (:constructor %make-screen))
  view
  modified-p)

(defun make-screen (view)
  (%make-screen :view view))

(defun screen-delete (screen)
  (lem-if:delete-view (implementation) (screen-view screen)))

(defun screen-clear (screen)
  (screen-modify screen)
  (lem-if:clear (implementation) (screen-view screen)))

(defun screen-modify (screen)
  (setf (screen-modified-p screen) t))

(defun screen-set-size (screen width height)
  (screen-modify screen)
  (lem-if:set-view-size (implementation) (screen-view screen) width height))

(defun screen-set-pos (screen x y)
  (screen-modify screen)
  (lem-if:set-view-pos (implementation) (screen-view screen) x y))

(defun update-screen-cache (screen buffer)
  (setf (screen-modified-p screen)
        nil))
