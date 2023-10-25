(defpackage :lem-sdl2/display
  (:use :cl))
(in-package :lem-sdl2/display)

(defclass display ()
  ((mutex :initform (bt:make-lock "lem-sdl2 display mutex")
          :reader display-mutex)
   (font-config :initarg :font-config
                :accessor display-font-config)
   (font :initarg :font
         :type font
         :accessor display-font)
   (renderer :initarg :renderer
             :reader display-renderer)
   (texture :initarg :texture
            :accessor display-texture)
   (window :initarg :window
           :reader display-window)
   (char-width :initarg :char-width
               :accessor display-char-width)
   (char-height :initarg :char-height
                :accessor display-char-height)
   (foreground-color :initform (lem:make-color #xff #xff #xff)
                     :accessor display-foreground-color)
   (background-color :initform (lem:make-color 0 0 0)
                     :accessor display-background-color)
   (focus :initform nil
          :accessor display-focus-p)
   (redraw-at-least-once :initform nil
                         :accessor display-redraw-at-least-once-p)
   (scale :initform '(1 1)
          :initarg :scale
          :accessor display-scale)))

(defmethod display-latin-font ((display display))
  (lem-sdl2/font:font-latin-normal-font (display-font display)))

(defmethod display-latin-bold-font ((display display))
  (lem-sdl2/font:font-latin-bold-font (display-font display)))

(defmethod display-cjk-normal-font ((display display))
  (lem-sdl2/font:font-cjk-normal-font (display-font display)))

(defmethod display-cjk-bold-font ((display display))
  (lem-sdl2/font:font-cjk-bold-font (display-font display)))

(defmethod display-emoji-font ((display display))
  (lem-sdl2/font:font-emoji-font (display-font display)))

(defmethod display-braille-font ((display display))
  (lem-sdl2/font:font-braille-font (display-font display)))

(defmethod display-background-color ((display display))
  (or (lem:parse-color lem-if:*background-color-of-drawing-window*)
      (slot-value display 'background-color)))

(defun call-with-renderer (display function)
  (sdl2:in-main-thread ()
    (bt:with-recursive-lock-held ((display-mutex display))
      (funcall function))))

(defmacro with-renderer ((display) &body body)
  `(call-with-renderer ,display (lambda () ,@body)))

(defmethod clear ((display display))
  (sdl2:set-render-target (display-renderer display) (display-texture display))
  (set-render-color display (display-background-color display))
  (sdl2:render-fill-rect (display-renderer display) nil))

(defmethod get-display-font ((display display) &key type bold character)
  (check-type type lem-core::char-type)
  (cond ((eq type :control)
         (display-latin-font display))
        ((eq type :icon)
         (or (and character (lem-sdl2/icon-font:icon-font
                             character
                             (lem-sdl2/font:font-config-size (display-font-config display))))
             (display-emoji-font display)))
        ((eq type :emoji)
         (display-emoji-font display))
        ((eq type :braille)
         (display-braille-font display))
        (bold
         (if (eq type :latin)
             (display-latin-bold-font display)
             (display-cjk-bold-font display)))
        (t
         (if (eq type :latin)
             (display-latin-font display)
             (display-cjk-normal-font display)))))

(defmethod scaled-char-width ((display display) x)
  (let ((scale-x (round (first (display-scale display)))))
    (floor (* scale-x x) (display-char-width display))))

(defmethod scaled-char-height ((display display) y)
  (let ((scale-y (round (second (display-scale display)))))
    (floor (* scale-y y) (display-char-height display))))

(defmethod update-display ((display display))
  (sdl2:render-present (display-renderer display)))

(defmethod display-width ((display display))
  (nth-value 0 (sdl2:get-renderer-output-size (display-renderer display))))

(defmethod display-height ((display display))
  (nth-value 1 (sdl2:get-renderer-output-size (display-renderer display))))

(defmethod display-window-width ((display display))
  (nth-value 0 (sdl2:get-window-size (display-window display))))

(defmethod display-window-height ((display display))
  (nth-value 1 (sdl2:get-window-size (display-window display))))

(defmethod update-texture ((display display))
  (bt:with-lock-held ((display-mutex display))
    (sdl2:destroy-texture (display-texture display))
    (setf (display-texture display)
          (lem-sdl2/utils:create-texture (display-renderer display)
                                         (display-width display)
                                         (display-height display)))))

(defmethod set-render-color ((display display) color)
  (when color
    (sdl2:set-render-draw-color (display-renderer display)
                                (lem:color-red color)
                                (lem:color-green color)
                                (lem:color-blue color)
                                0)))

(defmethod notify-required-redisplay ((display display))
  (with-renderer (display)
    (when (display-redraw-at-least-once-p display)
      (setf (display-redraw-at-least-once-p display) nil)
      (sdl2:set-render-target (display-renderer display) (display-texture display))
      (set-render-color display (display-background-color display))
      (sdl2:render-clear (display-renderer display))
      #+darwin
      (adapt-high-dpi-display-scale)
      #+darwin
      (adapt-high-dpi-font-size)
      (lem:update-on-display-resized))))

(defmethod render-fill-rect ((display display) x y width height &key color)
  (let ((x (* x (display-char-width display)))
        (y (* y (display-char-height display)))
        (width (* width (display-char-width display)))
        (height (* height (display-char-height display))))
    (sdl2:with-rects ((rect x y width height))
      (set-render-color display color)
      (sdl2:render-fill-rect (display-renderer display) rect))))

(defmethod render-line (display x1 y1 x2 y2 &key color)
  (set-render-color display color)
  (sdl2:render-draw-line (display-renderer display) x1 y1 x2 y2))

(defmethod render-fill-rect-by-pixels ((display display) x y width height &key color)
  (sdl2:with-rects ((rect x y width height))
    (set-render-color display color)
    (sdl2:render-fill-rect (display-renderer display) rect)))

(defmethod render-border ((display display) x y w h &key without-topline)
  (let* ((x1 (- (* x (display-char-width display)) (floor (display-char-width display) 2)))
         (y1 (- (* y (display-char-height display)) (floor (display-char-height display) 2)))
         (x2 (1- (+ x1 (* (+ w 1) (display-char-width display)))))
         (y2 (+ y1 (* (+ h 1) (display-char-height display)))))
    (sdl2:with-rects ((rect x1 y1 (- x2 x1) (- y2 y1)))
      (set-render-color display (display-background-color display))
      (sdl2:render-fill-rect (display-renderer display) rect))
    (sdl2:with-points ((upleft x1 y1)
                       (downleft x1 y2)
                       (downright x2 y2)
                       (upright x2 y1))
      (if without-topline
          (progn
            (set-render-color display (display-foreground-color display))
            (sdl2:render-draw-lines (display-renderer display) (sdl2:points* downleft upleft) 2)
            (set-render-color display (display-foreground-color display))
            (sdl2:render-draw-lines (display-renderer display) (sdl2:points* upleft upright) 2))
          (progn
            (set-render-color display (display-foreground-color display))
            (sdl2:render-draw-lines (display-renderer display) (sdl2:points* downleft upleft upright) 3)))
      (set-render-color display (display-foreground-color display))
      (sdl2:render-draw-lines (display-renderer display) (sdl2:points* upright downright downleft) 3))))

(defmethod render-margin-line ((display display) x y height)
  (let ((attribute (lem:ensure-attribute 'lem:modeline-inactive)))
    (render-fill-rect display
                      (1- x)
                      y
                      1
                      height
                      :color (lem-core:attribute-background-color attribute))
    (render-fill-rect-by-pixels display
                                (+ (* (1- x) (display-char-width display))
                                   (floor (display-char-width display) 2)
                                   -1)
                                (* y (display-char-height display))
                                2
                                (* height (display-char-height display))
                                :color (lem-core:attribute-foreground-color attribute))))

(defmethod change-font ((display display) font-config &optional (save-font-size-p t))
  (let ((font-config (lem-sdl2/font:merge-font-config font-config (display-font-config display))))
    (lem-sdl2/font:close-font (display-font display))
    (let ((font (lem-sdl2/font:open-font font-config)))
      (setf (display-char-width display) (lem-sdl2/font:font-char-width font)
            (display-char-height display) (lem-sdl2/font:font-char-height font))
      (setf (display-font-config display) font-config)
      (setf (display-font display) font))
    (when save-font-size-p
      (lem-sdl2/font:save-font-size font-config (first (display-scale display))))
    (lem-sdl2/icon-font:clear-icon-font-cache)
    (lem-sdl2/text-surface-cache:clear-text-surface-cache)
    (lem:send-event :resize)))

(defmethod create-view-texture ((display display) width height)
  (lem-sdl2/utils:create-texture (display-renderer display)
                                 (* width (display-char-width display))
                                 (* height (display-char-height display))))
