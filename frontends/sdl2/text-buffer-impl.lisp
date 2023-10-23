(defpackage :lem-sdl2/text-buffer-impl
  (:use :cl)
  (:import-from :lem-core
                :control-character-object
                :cursor-attribute-p
                :emoji-object
                :eol-cursor-object
                :eol-cursor-object-color
                :extend-to-eol-object
                :extend-to-eol-object-color
                :folder-object
                :icon-object
                :image-object
                :image-object-height
                :image-object-image
                :image-object-width
                :line-end-object
                :line-end-object-offset
                :text-object
                :text-object-attribute
                :text-object-string
                :text-object-surface
                :text-object-type
                :void-object
                :window-view-height
                :window-view-width
                :text-object))
(in-package :lem-sdl2/text-buffer-impl)

(defmethod lem-if:view-width ((implementation lem-sdl2::sdl2) view)
  (* (lem-sdl2::char-width) (lem-sdl2::view-width view)))

(defmethod lem-if:view-height ((implementation lem-sdl2::sdl2) view)
  (* (lem-sdl2::char-height) (lem-sdl2::view-height view)))

(defun set-cursor-position (view x y)
  (setf (lem-sdl2::view-last-cursor-x view) x
        (lem-sdl2::view-last-cursor-y view) y))

(defgeneric get-surface (drawing-object))

(defmethod get-surface :around (drawing-object)
  (or (text-object-surface drawing-object)
      (setf (text-object-surface drawing-object)
            (call-next-method))))

(defun get-font (&key attribute type bold)
  (or (lem-core::attribute-font attribute)
      (lem-sdl2::get-display-font lem-sdl2::*display* :type type :bold bold)))

(defun make-text-surface (string attribute type)
  (cffi:with-foreign-string (c-string string)
    (let ((foreground (lem-core:attribute-foreground-with-reverse attribute)))
      (sdl2-ttf:render-utf8-blended
       (get-font :attribute attribute
                 :type type
                 :bold (and attribute (lem:attribute-bold attribute)))
       c-string
       (lem:color-red foreground)
       (lem:color-green foreground)
       (lem:color-blue foreground)
       0))))

(defun make-text-surface-with-cache (string attribute type)
  (or (lem-sdl2/text-surface-cache:get-text-surface-cache string attribute type)
      (let ((surface (make-text-surface string attribute type)))
        (lem-sdl2/text-surface-cache:register-text-surface-cache string attribute type surface)
        surface)))

(defmethod get-surface ((drawing-object text-object))
  (let ((string (text-object-string drawing-object))
        (attribute (text-object-attribute drawing-object))
        (type (text-object-type drawing-object)))
    (make-text-surface-with-cache string attribute type)))

(defmethod get-surface ((drawing-object icon-object))
  (let* ((string (text-object-string drawing-object))
         (attribute (text-object-attribute drawing-object))
         (font (lem-sdl2::icon-font (char (text-object-string drawing-object) 0)))
         (foreground (lem-core:attribute-foreground-with-reverse attribute)))
    (cffi:with-foreign-string (c-string string)
      (sdl2-ttf:render-utf8-blended font
                                    c-string
                                    (lem:color-red foreground)
                                    (lem:color-green foreground)
                                    (lem:color-blue foreground)
                                    0))))

(defmethod get-surface ((drawing-object folder-object))
  (sdl2-image:load-image
   (lem-sdl2::get-resource-pathname
    "resources/open-folder.png")))

(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object void-object))
  0)

(defmethod object-width ((drawing-object text-object))
  (sdl2:surface-width (get-surface drawing-object)))

(defmethod object-width ((drawing-object control-character-object))
  (* 2 (lem-sdl2::char-width)))

(defmethod object-width ((drawing-object icon-object))
  (sdl2:surface-width (get-surface drawing-object)))

(defmethod object-width ((drawing-object folder-object))
  (* 2 (lem-sdl2::char-width)))

(defmethod object-width ((drawing-object emoji-object))
  (* (lem-sdl2::char-width) 2 (length (text-object-string drawing-object))))

(defmethod object-width ((drawing-object eol-cursor-object))
  0)

(defmethod object-width ((drawing-object extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object line-end-object))
  (sdl2:surface-width (get-surface drawing-object)))

(defmethod object-width ((drawing-object image-object))
  (or (image-object-width drawing-object)
      (sdl2:surface-width (image-object-image drawing-object))))


(defgeneric object-height (drawing-object))

(defmethod object-height ((drawing-object void-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object text-object))
  (sdl2:surface-height (get-surface drawing-object)))

(defmethod object-height ((drawing-object icon-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object control-character-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object folder-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object emoji-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object eol-cursor-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object extend-to-eol-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object line-end-object))
  (lem-sdl2::char-height))

(defmethod object-height ((drawing-object image-object))
  (or (image-object-height drawing-object)
      (sdl2:surface-height (image-object-image drawing-object))))

(defmethod lem-if:object-width ((implementation lem-sdl2::sdl2) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((implementation lem-sdl2::sdl2) drawing-object)
  (object-height drawing-object))

;;; draw-object
(defmethod draw-object ((drawing-object void-object) x bottom-y view)
  0)

(defmethod draw-object ((drawing-object text-object) x bottom-y view)
  (let* ((surface-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (attribute (text-object-attribute drawing-object))
         (background (lem-core:attribute-background-with-reverse attribute))
         (texture (sdl2:create-texture-from-surface
                   (lem-sdl2::current-renderer)
                   (get-surface drawing-object)))
         (y (- bottom-y surface-height)))
    (when (and attribute (cursor-attribute-p attribute))
      (set-cursor-position view x y))
    (sdl2:with-rects ((rect x y surface-width surface-height))
      (lem-sdl2::set-render-color lem-sdl2::*display* background)
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
                                              (lem-sdl2::attribute-foreground-color attribute))))))
    surface-width))

(defmethod draw-object ((drawing-object eol-cursor-object) x bottom-y view)
  (lem-sdl2::set-render-color lem-sdl2::*display* (eol-cursor-object-color drawing-object))
  (let ((y (- bottom-y (object-height drawing-object))))
    (set-cursor-position view x y)
    (sdl2:with-rects ((rect x
                            y
                            (lem-sdl2::char-width)
                            (object-height drawing-object)))
      (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect)))
  (object-width drawing-object))

(defmethod draw-object ((drawing-object extend-to-eol-object) x bottom-y view)
  (lem-sdl2::set-render-color lem-sdl2::*display* (extend-to-eol-object-color drawing-object))
  (sdl2:with-rects ((rect x
                          (- bottom-y (lem-sdl2::char-height))
                          (- (lem-if:view-width (lem-core:implementation) view) x)
                          (lem-sdl2::char-height)))
    (sdl2:render-fill-rect (lem-sdl2::current-renderer)
                           rect))
  (object-width drawing-object))

(defmethod draw-object ((drawing-object line-end-object) x bottom-y view)
  (call-next-method drawing-object
                    (+ x
                       (* (line-end-object-offset drawing-object)
                          (lem-sdl2::char-width)))
                    bottom-y))

(defmethod draw-object ((drawing-object image-object) x bottom-y view)
  (let* ((surface-width (object-width drawing-object))
         (surface-height (object-height drawing-object))
         (texture (sdl2:create-texture-from-surface (lem-sdl2::current-renderer)
                                                    (image-object-image drawing-object)))
         (y (- bottom-y surface-height)))
    (lem-sdl2::render-texture (lem-sdl2::current-renderer) texture x y surface-width surface-height)
    (sdl2:destroy-texture texture)
    surface-width))

(defun redraw-physical-line (view x y objects height)
  (loop :with current-x := x
        :for object :in objects
        :do (incf current-x (draw-object object current-x (+ y height) view))))

(defun clear-to-end-of-line (view x y height)
  (sdl2:with-rects ((rect x y (- (lem-if:view-width (lem-core:implementation) view) x) height))
    (lem-sdl2::set-render-color lem-sdl2::*display* (lem-sdl2::display-background-color lem-sdl2::*display*))
    (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect)))

(defmethod lem-if:render-line ((implementation lem-sdl2::sdl2) view x y objects height)
  (clear-to-end-of-line view 0 y height)
  (redraw-physical-line view x y objects height))

(defmethod lem-if:clear-to-end-of-window ((implementation lem-sdl2::sdl2) window y)
  (lem-sdl2::set-render-color
   lem-sdl2::*display*
   (lem-sdl2::display-background-color lem-sdl2::*display*))
  (sdl2:with-rects ((rect 0
                          y
                          (window-view-width window)
                          (- (window-view-height window) y)))
    (sdl2:render-fill-rect (lem-sdl2::current-renderer) rect)))

(defmethod lem-core:redraw-buffer :before ((implementation lem-sdl2::sdl2) buffer window force)
  (sdl2:set-render-target (lem-sdl2::current-renderer)
                          (lem-sdl2::view-texture (lem:window-view window))))

(defmethod lem-core:redraw-buffer :around ((implementation lem-sdl2::sdl2) buffer window force)
  (sdl2:in-main-thread ()
    (call-next-method)))
