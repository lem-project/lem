(defpackage :lem-sdl2/text-buffer-impl
  (:use :cl))
(in-package :lem-sdl2/text-buffer-impl)

(defmethod lem-if:view-width ((implementation lem-sdl2::sdl2) view)
  (* (lem-sdl2::char-width) (lem-sdl2::view-width view)))

(defmethod lem-if:view-height ((implementation lem-sdl2::sdl2) view)
  (* (lem-sdl2::char-height) (lem-sdl2::view-height view)))

(defun set-cursor-position (window x y)
  (let ((view (lem:window-view window)))
    (setf (lem-sdl2::view-last-cursor-x view) x
          (lem-sdl2::view-last-cursor-y view) y)))

(defun attribute-font (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when attribute
      (lem:attribute-value attribute 'font))))

(defun get-font (&key attribute type bold)
  (or (alexandria:when-let (attribute (and attribute (lem:ensure-attribute attribute)))
        (attribute-font attribute))
      (lem-sdl2::get-display-font lem-sdl2::*display* :type type :bold bold)))

(defun render-string (string attribute type)
  (let ((foreground (lem-core::attribute-foreground-with-reverse attribute))
        (bold (and attribute (lem:attribute-bold attribute))))
    (cffi:with-foreign-string (c-string string)
      (sdl2-ttf:render-utf8-blended (get-font :attribute attribute
                                              :type type
                                              :bold bold)
                                    c-string
                                    (lem:color-red foreground)
                                    (lem:color-green foreground)
                                    (lem:color-blue foreground)
                                    0))))

(defmethod get-surface ((text-object lem-core/display-3::text-object))
  (or (lem-core/display-3::text-object-surface text-object)
      (setf (lem-core/display-3::text-object-surface text-object)
            (render-string (lem-core/display-3::text-object-string text-object)
                           (lem-core/display-3::text-object-attribute text-object)
                           (lem-core/display-3::text-object-type text-object)))))

(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object lem-core/display-3::void-object))
  0)

(defmethod object-width ((drawing-object lem-core/display-3::text-object))
  (if (eq :emoji (lem-core/display-3::text-object-type drawing-object))
      (* (lem-sdl2::char-width) 2 (length (lem-core/display-3::text-object-string drawing-object)))
      (sdl2:surface-width (get-surface drawing-object))))

(defmethod object-width ((drawing-object lem-core/display-3::eol-cursor-object))
  0)

(defmethod object-width ((drawing-object lem-core/display-3::extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object lem-core/display-3::line-end-object))
  (sdl2:surface-width (get-surface drawing-object)))

(defmethod object-width ((drawing-object lem-core/display-3::image-object))
  (or (lem-core/display-3::image-object-width drawing-object)
      (sdl2:surface-width (lem-core/display-3::image-object-image drawing-object))))


(defgeneric object-height (drawing-object))

(defmethod object-height ((drawing-object lem-core/display-3::void-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object lem-core/display-3::text-object))
  (if (eq :emoji (lem-core/display-3::text-object-type drawing-object))
      (lem-sdl2::char-height)
      (sdl2:surface-height (get-surface drawing-object))))

(defmethod object-height ((drawing-object lem-core/display-3::eol-cursor-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object lem-core/display-3::extend-to-eol-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object lem-core/display-3::line-end-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object lem-core/display-3::image-object))
  (or (lem-core/display-3::image-object-height drawing-object)
      (sdl2:surface-height (lem-core/display-3::image-object-image drawing-object))))

(defmethod lem-if:object-width ((implementation lem-sdl2::sdl2) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((implementation lem-sdl2::sdl2) drawing-object)
  (object-height drawing-object))

;;; draw-object
(defmethod draw-object ((drawing-object lem-core/display-3::void-object) x bottom-y window)
  (values))

(defmethod draw-object ((drawing-object lem-core/display-3::text-object) x bottom-y window)
  (let* ((surface-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (attribute (lem-core/display-3::text-object-attribute drawing-object))
         (background (lem-core::attribute-background-with-reverse attribute))
         (texture (sdl2:create-texture-from-surface
                   (lem-sdl2::current-renderer)
                   (get-surface drawing-object)))
         (y (- bottom-y surface-height)))
    (when (and attribute (lem-core/display-3::cursor-attribute-p attribute))
      (set-cursor-position window x y))
    (sdl2:with-rects ((rect x y surface-width surface-height))
      (lem-sdl2::set-color background)
      (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect))
    (lem-sdl2::render-texture (lem-sdl2::current-renderer)
                              texture
                              x
                              y
                              surface-width
                              surface-height)
    (sdl2:destroy-texture texture)
    (when (and attribute
               (lem:attribute-underline attribute))
      (lem-sdl2::render-line x
                             (1- (+ y surface-height))
                             (+ x surface-width)
                             (1- (+ y surface-height))
                             :color (let ((underline (lem:attribute-underline attribute)))
                                      (if (eq underline t)
                                          (lem-sdl2::attribute-foreground-color attribute)
                                          (or (lem:parse-color underline)
                                              (lem-sdl2::attribute-foreground-color attribute))))))))

(defmethod draw-object ((drawing-object lem-core/display-3::eol-cursor-object) x bottom-y window)
  (lem-sdl2::set-color (lem-core/display-3::eol-cursor-object-color drawing-object))
  (let ((y (- bottom-y (object-height drawing-object))))
    (set-cursor-position window x y)
    (sdl2:with-rects ((rect x
                            y
                            (lem-sdl2::char-width)
                            (object-height drawing-object)))
      (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect))))

(defmethod draw-object ((drawing-object lem-core/display-3::extend-to-eol-object) x bottom-y window)
  (lem-sdl2::set-color (lem-core/display-3::extend-to-eol-object-color drawing-object))
  (sdl2:with-rects ((rect x
                          (- bottom-y (lem-sdl2::char-height))
                          (- (lem-core/display-3::window-view-width window) x)
                          (lem-sdl2::char-height)))
    (sdl2:render-fill-rect (lem-sdl2::current-renderer)
                           rect)))

(defmethod draw-object ((drawing-object lem-core/display-3::line-end-object) x bottom-y window)
  (call-next-method drawing-object
                    (+ x
                       (* (lem-core/display-3::line-end-object-offset drawing-object)
                          (lem-sdl2::char-width)))
                    bottom-y))

(defmethod draw-object ((drawing-object lem-core/display-3::image-object) x bottom-y window)
  (let* ((surface-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (texture (sdl2:create-texture-from-surface (lem-sdl2::current-renderer)
                                                    (lem-core/display-3::image-object-image drawing-object)))
         (y (- bottom-y surface-height)))
    (lem-sdl2::render-texture (lem-sdl2::current-renderer) texture x y surface-width surface-height)
    (sdl2:destroy-texture texture)))

(defun redraw-physical-line (window x y objects height)
  (loop :for current-x := x :then (+ current-x (object-width object))
        :for object :in objects
        :do (draw-object object current-x (+ y height) window)))

(defun clear-to-end-of-line (window x y height)
  (sdl2:with-rects ((rect x y (- (lem-core/display-3::window-view-width window) x) height))
    (lem-sdl2::set-render-color lem-sdl2::*display* (lem-sdl2::display-background-color lem-sdl2::*display*))
    (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect)))

(defmethod lem-core/display-3::%render-line ((implementation lem-sdl2::sdl2) window x y objects height)
  (clear-to-end-of-line window 0 y height)
  (redraw-physical-line window x y objects height))

(defmethod lem-if:clear-to-end-of-window ((implementation lem-sdl2::sdl2) window y)
  (lem-sdl2::set-render-color
   lem-sdl2::*display*
   (lem-sdl2::display-background-color lem-sdl2::*display*))
  (sdl2:with-rects ((rect 0
                          y
                          (lem-core/display-3::window-view-width window)
                          (- (lem-core/display-3::window-view-height window) y)))
    (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect)))

(defmethod lem-core::redraw-buffer :around ((implementation lem-sdl2::sdl2)
                                            (buffer lem-core/display-3::text-buffer-v2)
                                            window
                                            force)
  (sdl2:in-main-thread ()
    (sdl2:set-render-target (lem-sdl2::current-renderer)
                            (lem-sdl2::view-texture (lem:window-view window)))
    (call-next-method)))
