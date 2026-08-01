(defpackage :lem-server/view
  (:use :cl)
  (:export :view
           :make-view
           :view-window
           :view-id
           :view-cached-id-hash
           :view-x
           :view-y
           :view-width
           :view-height
           :view-pixel-x
           :view-pixel-y
           :view-pixel-width
           :view-pixel-height
           :view-px-x
           :view-px-y
           :view-px-width
           :view-px-height
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
  ;; Memoized {"id": id} hash for hot-path messages (put, clear-eol).
  ;; Immutable once built — id never changes — so it is safe to reuse the
  ;; same instance across every message in a frame.  See VIEW-ID-HASH.
  (cached-id-hash nil)
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

(defun cell-pixel-size ()
  "The pixel size of one character cell, as (values WIDTH HEIGHT).
Never NIL here: this frontend starts from a guess and the client corrects it at login."
  (lem-if:cell-pixel-size (lem:implementation)))

(defun view-px-x (view)
  "VIEW's left edge in pixels."
  (or (view-pixel-x view)
      (* (view-x view) (nth-value 0 (cell-pixel-size)))))

(defun view-px-y (view)
  "VIEW's top edge in pixels."
  (or (view-pixel-y view)
      (* (view-y view) (nth-value 1 (cell-pixel-size)))))

(defun view-px-width (view)
  "VIEW's width in pixels."
  (or (view-pixel-width view)
      (* (view-width view) (nth-value 0 (cell-pixel-size)))))

(defun view-px-height (view)
  "VIEW's height in pixels, the edit area only, since the modeline is a surface of its own."
  (or (view-pixel-height view)
      (* (view-height view) (nth-value 1 (cell-pixel-size)))))

(defun move-view (view x y &optional pixel-x pixel-y)
  "Move VIEW to cell position X, Y, or to PIXEL-X / PIXEL-Y for an axis given in pixels.
Passing no pixel position clears any earlier one, so the view follows the cell grid again."
  (setf (view-x view) x
        (view-y view) y
        (view-pixel-x view) pixel-x
        (view-pixel-y view) pixel-y)
  (values))

(defun resize-view (view width height &optional pixel-width pixel-height)
  "Resize VIEW to WIDTH x HEIGHT cells, or to PIXEL-WIDTH / PIXEL-HEIGHT for a dimension given in
pixels. As in `move-view', passing no pixel size clears any earlier one."
  (setf (view-width view) width
        (view-height view) height
        (view-pixel-width view) pixel-width
        (view-pixel-height view) pixel-height)
  (values))

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view))
      ;; the cell geometry the core laid this view out on
      (yason:encode-object-element "x" (view-x view))
      (yason:encode-object-element "y" (view-y view))
      (yason:encode-object-element "width" (view-width view))
      (yason:encode-object-element "height" (view-height view))
      ;; and in pixels, always present, so the client never needs the cell size
      (yason:encode-object-element "pixelX" (view-px-x view))
      (yason:encode-object-element "pixelY" (view-px-y view))
      (yason:encode-object-element "pixelWidth" (view-px-width view))
      (yason:encode-object-element "pixelHeight" (view-px-height view))
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

