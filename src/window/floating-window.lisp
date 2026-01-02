(in-package :lem-core)

(defclass floating-window (window)
  ((border
    :initarg :border
    :initform 0
    :reader floating-window-border)
   (border-shape
    :type (member nil :drop-curtain)
    :initarg :border-shape
    :initform nil
    :reader floating-window-border-shape)
   (background-color
    :initarg :background-color
    :initform nil
    :reader floating-window-background-color)
   (focusable
    :initform nil
    :accessor floating-window-focusable-p)
   (pixel-x
    :initarg :pixel-x
    :initform nil
    :accessor floating-window-pixel-x
    :type (or null integer)
    :documentation "X position in pixels. If nil, calculated from character x.")
   (pixel-y
    :initarg :pixel-y
    :initform nil
    :accessor floating-window-pixel-y
    :type (or null integer)
    :documentation "Y position in pixels. If nil, calculated from character y.")
   (pixel-width
    :initarg :pixel-width
    :initform nil
    :accessor floating-window-pixel-width
    :type (or null integer)
    :documentation "Width in pixels. If nil, calculated from character width.")
   (pixel-height
    :initarg :pixel-height
    :initform nil
    :accessor floating-window-pixel-height
    :type (or null integer)
    :documentation "Height in pixels. If nil, calculated from character height.")))

(defmethod window-border ((window floating-window))
  (floating-window-border window))

(defmethod initialize-instance :before ((floating-window floating-window) &rest initargs)
  (declare (ignore initargs))
  (unless (support-floating-window (implementation))
    (error "floating window is not supported"))
  (notify-floating-window-modified (current-frame)))

(defmethod initialize-instance :after ((floating-window floating-window)
                                       &key (frame (current-frame)) &allow-other-keys)
  (add-floating-window frame floating-window))

(defmethod initialize-instance ((floating-window floating-window)
                                &rest initargs
                                &key use-border
                                &allow-other-keys)
  (apply #'call-next-method
         floating-window
         (if use-border
             (list* :border 1 initargs)
             initargs)))

(defun make-floating-window (&key (buffer (alexandria:required-argument :buffer))
                                  (x (alexandria:required-argument :x))
                                  (y (alexandria:required-argument :y))
                                  (width (alexandria:required-argument :width))
                                  (height (alexandria:required-argument :height))
                                  (pixel-x nil)
                                  (pixel-y nil)
                                  (pixel-width nil)
                                  (pixel-height nil)
                                  (use-modeline-p nil)
                                  (background-color nil)
                                  (use-border nil))
  "Create a floating window with optional pixel positioning.
X, Y, WIDTH, HEIGHT are required and specify character-unit coordinates.
PIXEL-X, PIXEL-Y, PIXEL-WIDTH, PIXEL-HEIGHT are optional pixel coordinates.
If pixel coordinates are nil, frontends may calculate from character units."
  (make-instance 'floating-window
                 :buffer buffer
                 :x x
                 :y y
                 :width width
                 :height height
                 :pixel-x pixel-x
                 :pixel-y pixel-y
                 :pixel-width pixel-width
                 :pixel-height pixel-height
                 :use-modeline-p use-modeline-p
                 :background-color background-color
                 :border (if use-border 1 0)))

(defmethod %delete-window ((window floating-window))
  (when (eq window (current-window))
    (editor-error "Can not delete this window"))
  (notify-floating-window-modified (current-frame))
  (remove-floating-windows (current-frame) window))

(defun floating-window-p (window)
  (typep window 'floating-window))

(defun floating-window-set-pixel-position (window pixel-x pixel-y)
  "Set the pixel position of a floating window.
This updates the window's pixel coordinates and notifies the frontend."
  (check-type window floating-window)
  (setf (floating-window-pixel-x window) pixel-x
        (floating-window-pixel-y window) pixel-y)
  (when (support-pixel-positioning-p (implementation))
    (lem-if:set-view-pos-pixels (implementation)
                                (window-view window)
                                (window-x window)
                                (window-y window)
                                pixel-x
                                pixel-y))
  (notify-floating-window-modified (current-frame)))

(defun floating-window-set-pixel-size (window pixel-width pixel-height)
  "Set the pixel size of a floating window.
This updates the window's pixel dimensions and notifies the frontend."
  (check-type window floating-window)
  (setf (floating-window-pixel-width window) pixel-width
        (floating-window-pixel-height window) pixel-height)
  (when (support-pixel-positioning-p (implementation))
    (lem-if:set-view-size-pixels (implementation)
                                 (window-view window)
                                 (window-width window)
                                 (window-height window)
                                 pixel-width
                                 pixel-height))
  (notify-floating-window-modified (current-frame)))

(defun floating-window-pixel-bounds (window)
  "Return the pixel bounds of a floating window.
Returns (values pixel-x pixel-y pixel-width pixel-height).
If pixel coordinates are not set, calculates from character coordinates."
  (check-type window floating-window)
  (let ((char-width (lem-if:get-char-width (implementation)))
        (char-height (lem-if:get-char-height (implementation))))
    (values (or (floating-window-pixel-x window)
                (* (window-x window) char-width))
            (or (floating-window-pixel-y window)
                (* (window-y window) char-height))
            (or (floating-window-pixel-width window)
                (* (window-width window) char-width))
            (or (floating-window-pixel-height window)
                (* (window-height window) char-height)))))
