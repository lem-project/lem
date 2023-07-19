(in-package :lem-core)

(defstruct (screen (:constructor %make-screen))
  view
  modeline-elements
  left-lines
  left-width
  old-left-width
  lines
  old-lines
  wrap-lines
  width
  modified-p
  last-buffer-name
  last-buffer-modified-tick
  (horizontal-scroll-start 0)
  (last-print-cursor-x 0)
  (last-print-cursor-y 0))

(defun make-screen (view width height)
  (%make-screen :view view
                :width width
                :left-lines (make-array (max 0 height) :initial-element nil)
                :lines (make-array (max 0 height) :initial-element nil)
                :old-lines (make-array (max 0 height) :initial-element nil)))

(defun screen-delete (screen)
  (lem-if:delete-view (implementation) (screen-view screen)))

(defun screen-clear (screen)
  (screen-modify screen)
  (lem-if:clear (implementation) (screen-view screen)))

(defun screen-height (screen)
  (length (screen-lines screen)))

(defun screen-modify (screen)
  (setf (screen-modified-p screen) t))

(defun screen-set-size (screen width height)
  (screen-modify screen)
  (lem-if:set-view-size (implementation) (screen-view screen) width height)
  (setf (screen-left-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-old-lines screen)
        (make-array height :initial-element nil))
  (setf (screen-width screen)
        width))

(defun screen-set-pos (screen x y)
  (screen-modify screen)
  (lem-if:set-view-pos (implementation) (screen-view screen) x y))

(defun update-screen-cache (screen buffer)
  (setf (screen-old-left-width screen)
        (screen-left-width screen))
  (setf (screen-last-buffer-name screen)
        (buffer-name buffer))
  (setf (screen-last-buffer-modified-tick screen)
        (buffer-modified-tick buffer))
  (setf (screen-modified-p screen)
        nil))

(defun required-whole-update-screen-p (screen)
  (or (screen-modified-p screen)
      (not (eql (screen-left-width screen)
                (screen-old-left-width screen)))))
