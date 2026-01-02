(defpackage :lem-server/view
  (:use :cl)
  (:export :view
           :make-view
           :view-window
           :view-id
           :view-x
           :view-y
           :view-width
           :view-height
           :view-pixel-x
           :view-pixel-y
           :view-pixel-width
           :view-pixel-height
           :view-use-modeline
           :view-kind
           :move-view
           :resize-view))
(in-package :lem-server/view)

(defvar *view-id-counter* 0)

(deftype border-shape ()
  '(member nil :drop-curtain :left-border))

(defstruct (view (:constructor %make-view))
  (id (incf *view-id-counter*))
  window
  x
  y
  width
  height
  (pixel-x nil :type (or null integer))
  (pixel-y nil :type (or null integer))
  (pixel-width nil :type (or null integer))
  (pixel-height nil :type (or null integer))
  use-modeline
  kind ; "tile" / "floating" / "header"
  border
  (border-shape nil :type border-shape))

(defun make-view (&rest args &key window x y width height
                                   pixel-x pixel-y pixel-width pixel-height
                                   use-modeline kind border border-shape)
  (declare (ignore window x y width height pixel-x pixel-y pixel-width pixel-height
                   use-modeline kind border border-shape))
  (apply #'%make-view args))

(defun move-view (view x y &optional pixel-x pixel-y)
  "Move view to new position. Pixel coordinates are optional."
  (setf (view-x view) x
        (view-y view) y)
  (when pixel-x (setf (view-pixel-x view) pixel-x))
  (when pixel-y (setf (view-pixel-y view) pixel-y)))

(defun resize-view (view width height &optional pixel-width pixel-height)
  "Resize view. Pixel dimensions are optional."
  (setf (view-width view) width
        (view-height view) height)
  (when pixel-width (setf (view-pixel-width view) pixel-width))
  (when pixel-height (setf (view-pixel-height view) pixel-height))
  (values))

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view))
      ;; Character-unit coordinates (for backward compatibility)
      (yason:encode-object-element "x" (view-x view))
      (yason:encode-object-element "y" (view-y view))
      (yason:encode-object-element "width" (view-width view))
      (yason:encode-object-element "height" (view-height view))
      ;; Pixel coordinates (new)
      (yason:encode-object-element "pixelX" (view-pixel-x view))
      (yason:encode-object-element "pixelY" (view-pixel-y view))
      (yason:encode-object-element "pixelWidth" (view-pixel-width view))
      (yason:encode-object-element "pixelHeight" (view-pixel-height view))
      ;; Other existing fields
      (yason:encode-object-element "use_modeline" (view-use-modeline view))
      (yason:encode-object-element "kind" (view-kind view))
      (yason:encode-object-element "type"
                                   (let ((buffer (lem:window-buffer (view-window view))))
                                     (if (typep buffer 'lem:html-buffer)
                                         "html"
                                         "editor")))
      (yason:encode-object-element "content"
                                   (let ((buffer (lem:window-buffer (view-window view))))
                                     (when (typep buffer 'lem:html-buffer)
                                       (lem:html-buffer-html buffer))))
      (yason:encode-object-element "border" (view-border view))
      (yason:encode-object-element "border_shape" (if (view-border-shape view)
                                                      (string-downcase (view-border-shape view)))))))

