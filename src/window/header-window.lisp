(in-package :lem-core)

(defclass header-window (window) ())

(defgeneric header-window-height (window)
  (:documentation "Return the preferred height in character cells for a header window.
Subclasses can specialize this to request more vertical space.")
  (:method ((window header-window))
    1))

(defmethod initialize-instance ((window header-window) &key &allow-other-keys)
  (with-slots (x y width height) window
    (setf x 0)
    (setf y (reduce #'+ (frame-header-windows (current-frame))
                    :key #'header-window-height
                    :initial-value 0))
    (setf width (display-width))
    (setf height (header-window-height window)))
  (add-header-window (current-frame) window)
  (notify-header-window-modified (current-frame))
  (call-next-method))

(defmethod %delete-window ((window header-window))
  (remove-header-window (current-frame) window)
  (notify-header-window-modified (current-frame)))

(defun header-window-p (window)
  (typep window 'header-window))
