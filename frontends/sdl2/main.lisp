(defpackage :lem-sdl2
  (:use :cl
        :lem-sdl2/key))
(in-package :lem-sdl2)

(defmacro with-bindings (bindings &body body)
  `(let ,bindings
     (let ((bt:*default-special-bindings*
             (list* ,@(loop :for (var) :in bindings
                            :collect `(cons ',var ,var))
                    bt:*default-special-bindings*)))
       ,@body)))

(defun create-texture (renderer width height)
  (sdl2:create-texture renderer
                       sdl2:+pixelformat-rgba8888+
                       sdl2-ffi:+sdl-textureaccess-target+
                       width
                       height))

(defclass sdl2 (lem:implementation)
  ()
  (:default-initargs
   :name :sdl2
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(defparameter *display-width* 100)
(defparameter *display-height* 40)
(defparameter *font-size* 20)

(defparameter *latin-font-file* "resources/NotoSansMono/NotoSansMono-Regular.ttf")
(defparameter *latin-bold-font-file* "resources/NotoSansMono/NotoSansMono-Bold.ttf")
(defparameter *unicode-font-file* "resources/NotoSansJP/NotoSansJP-Regular.otf")
(defparameter *unicode-bold-font-file* "resources/NotoSansJP/NotoSansJP-Bold.otf")

(defvar *display*)

(defclass display ()
  ((mutex :initform (bt:make-lock "lem-sdl2 display mutex")
          :reader display-mutex)
   (latin-font :initarg :latin-font
               :accessor display-latin-font)
   (latin-bold-font :initarg :latin-bold-font
                    :accessor display-latin-bold-font)
   (unicode-font :initarg :unicode-font
                 :accessor display-unicode-font)
   (unicode-bold-font :initarg :unicode-bold-font
                      :accessor display-unicode-bold-font)
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
                     :accessor display-background-color)))

(defun display-font (&key latin bold)
  (if bold
      (if latin
          (display-latin-bold-font *display*)
          (display-unicode-bold-font *display*))
      (if latin
          (display-latin-font *display*)
          (display-unicode-font *display*))))

(defun char-width ()
  (display-char-width *display*))

(defun char-height ()
  (display-char-height *display*))

(defstruct view
  window
  x
  y
  width
  height
  use-modeline)

(defun get-character-size (font)
  (let* ((surface (sdl2-ttf:render-text-blended font "A" 0 0 0 0))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (sdl2:free-surface surface)
    (list width height)))

(defun render-line (x1 y1 x2 y2 &key color)
  (set-render-color color)
  (sdl2:render-draw-line (display-renderer *display*) x1 y1 x2 y2))

(defun render (renderer texture width height x y)
  (sdl2:with-rects ((dest-rect x y width height))
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect nil
                         :dest-rect dest-rect
                         :flip (list :none))))

(defun render-text (text x y &key color bold)
  (let ((x (* x (char-width)))
        (y (* y (char-height))))
    (loop :for c :across text
          :for i :from 0
          :for latin-p := (<= (char-code c) 128)
          :do (cffi:with-foreign-string (c-string (string c))
                (let* ((red (lem:color-red color))
                       (green (lem:color-green color))
                       (blue (lem:color-blue color))
                       (surface (sdl2-ttf:render-utf8-blended (display-font :latin latin-p :bold bold)
                                                            c-string
                                                            red
                                                            green
                                                            blue
                                                            0))
                       (text-width (sdl2:surface-width surface))
                       (text-height (sdl2:surface-height surface))
                       (texture (sdl2:create-texture-from-surface (display-renderer *display*)
                                                                  surface)))
                  (render (display-renderer *display*) texture text-width text-height x y)
                  (sdl2:destroy-texture texture)))
              (incf x (if latin-p
                          (char-width)
                          (* (char-width) 2))))))

(defun render-fill-text (text x y &key attribute)
  (let ((width (lem:string-width text))
        (underline (and attribute (lem:attribute-underline-p attribute)))
        (bold (and attribute (lem:attribute-bold-p attribute)))
        (reverse (and attribute (lem:attribute-reverse-p attribute))))
    (let ((background-color (if reverse
                                (attribute-foreground-color attribute)
                                (attribute-background-color attribute)))
          (foreground-color (if reverse
                                (attribute-background-color attribute)
                                (attribute-foreground-color attribute))))
      (fill-rect x y width 1 :color background-color)
      (render-text text x y :color foreground-color :bold bold)
      (when underline
        (render-line (* x (char-width))
                     (- (* (1+ y) (char-height)) 1)
                     (* (+ x width) (char-width))
                     (- (* (1+ y) (char-height)) 1)
                     :color foreground-color)))))

(defun set-render-color (color)
  (sdl2:set-render-draw-color (display-renderer *display*) (lem:color-red color)
                              (lem:color-green color)
                              (lem:color-blue color)
                              0))

(defun fill-rect (x y width height &key color)
  (let ((x (* x (char-width)))
        (y (* y (char-height)))
        (width (* width (char-width)))
        (height (* height (char-height))))
    (sdl2:with-rects ((rect x y width height))
      (set-render-color color)
      (sdl2:render-fill-rect (display-renderer *display*) rect))))

(defun fill-rect-by-pixels (x y width height &key color)
  (sdl2:with-rects ((rect x y width height))
    (set-render-color color)
    (sdl2:render-fill-rect (display-renderer *display*) rect)))

(defun render-border (x y w h)
  (sdl2:with-rects ((up-rect (- (* x (char-width)) (floor (char-width) 2))
                             (- (* y (char-height)) (floor (char-height) 2))
                             (* (+ w 1) (char-width))
                             (floor (char-height) 2))
                    (left-rect (- (* x (char-width)) (floor (char-width) 2))
                               (- (* y (char-height)) (floor (char-height) 2))
                               (floor (char-width) 2)
                               (* (+ h 1) (char-height)))
                    (right-rect (* (+ x w) (char-width))
                                (+ (* (1- y) (char-height)) (floor (char-height) 2))
                                (floor (char-width) 2)
                                (* (+ h 1) (char-height)))
                    (down-rect (- (* x (char-width)) (floor (char-width) 2))
                               (* (+ y h) (char-height))
                               (* (+ w 1) (char-width))
                               (floor (char-height) 2))

                    (border-rect (- (* x (char-width)) (floor (char-width) 2))
                                 (- (* y (char-height)) (floor (char-height) 2))
                                 (* (+ 1 w) (char-width))
                                 (* (+ 1 h) (char-height))))

    (set-render-color (display-background-color *display*))
    (sdl2:render-fill-rect (display-renderer *display*) up-rect)
    (sdl2:render-fill-rect (display-renderer *display*) down-rect)
    (sdl2:render-fill-rect (display-renderer *display*) left-rect)
    (sdl2:render-fill-rect (display-renderer *display*) right-rect)

    (set-render-color (display-foreground-color *display*))
    (sdl2:render-draw-rect (display-renderer *display*) border-rect)))

(defun update-display (display)
  (sdl2:render-present (display-renderer display)))

(defun display-width (display)
  (nth-value 0 (sdl2:get-window-size (display-window display))))

(defun display-height (display)
  (nth-value 1 (sdl2:get-window-size (display-window display))))

(defun update-texture (display)
  (bt:with-lock-held ((display-mutex display))
    (sdl2:destroy-texture (display-texture display))
    (setf (display-texture display)
          (create-texture (display-renderer display)
                          (display-width display)
                          (display-height display)))))

(defvar *modifier* (make-modifier))

(defun keydown (keysym)
  (update-modifier *modifier* keysym)
  (alexandria:when-let (key (keysym-to-key keysym))
    (lem:send-event key)))

(defun keyup (keysym)
  (update-modifier *modifier* keysym))

(defun textinput (text)
  (loop :for c :across text
        :do (multiple-value-bind (sym converted)
                (convert-to-sym c)
              (unless converted
                (lem:send-event
                 (make-key-with-modifier *modifier*
                                         (or sym (string c))))))))

(defun call-with-renderer (function)
  (bt:with-lock-held ((display-mutex *display*))
    (funcall function)))

(defmacro with-renderer (() &body body)
  `(call-with-renderer (lambda () ,@body)))

(defun create-display (function)
  (sdl2:with-init (:video)

    (sdl2-ttf:init)

    (let ((latin-font
            (sdl2-ttf:open-font (asdf:system-relative-pathname
                                 :lem-sdl2 *latin-font-file*)
                                *font-size*))
          (latin-bold-font
            (sdl2-ttf:open-font (asdf:system-relative-pathname
                                 :lem-sdl2 *latin-bold-font-file*)
                                *font-size*))
          (unicode-font
            (sdl2-ttf:open-font (asdf:system-relative-pathname
                                 :lem-sdl2 *unicode-font-file*)
                                *font-size*))
          (unicode-bold-font
            (sdl2-ttf:open-font (asdf:system-relative-pathname
                                 :lem-sdl2 *unicode-bold-font-file*)
                                *font-size*)))
      (destructuring-bind (char-width char-height) (get-character-size latin-font)
        (let ((window-width (* *display-width* char-width))
              (window-height (* *display-height* char-height)))
          (sdl2:with-window (window :title "Lem"
                                    :w window-width
                                    :h window-height
                                    :flags '(:shown :resizable))
            (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
              (let ((texture (create-texture renderer
                                             window-width
                                             window-height)))
                (with-bindings ((*display* (make-instance 'display
                                                          :latin-font latin-font
                                                          :latin-bold-font latin-bold-font
                                                          :unicode-font unicode-font
                                                          :unicode-bold-font unicode-bold-font
                                                          :renderer renderer
                                                          :window window
                                                          :texture texture
                                                          :char-width char-width
                                                          :char-height char-height)))
                  (sdl2:start-text-input)
                  (funcall function)
                  (sdl2:with-event-loop (:method :wait)
                    (:quit ()
                     t)
                    (:textinput (:text text)
                     (textinput text))
                    (:keydown (:keysym keysym)
                     (keydown keysym))
                    (:keyup (:keysym keysym)
                     (keyup keysym))
                    (:windowevent (:event event)
                     (when (equal event sdl2-ffi:+sdl-windowevent-resized+)
                       (update-texture *display*)
                       (lem:send-event :resize)))
                    (:idle ())))))))))))

(defun call-with-debug (log-function body-function)
  (funcall log-function)
  (handler-bind ((error (lambda (e)
                          (log:info "~A"
                                    (with-output-to-string (out)
                                      (format out "~A~%" e)
                                      (uiop:print-backtrace :condition e :stream out))))))
    (funcall body-function)))

(defun do-log (&rest args)
  (declare (ignorable args))
  ;(log:info "~{~A~^ ~}" args)
  )

(defmacro with-debug ((&rest args) &body body)
  `(call-with-debug (lambda () (do-log ,@args))
                    (lambda () ,@body)))

(defmethod lem-if:invoke ((implementation sdl2) function)
  (create-display (lambda ()
                    (let ((editor-thread
                            (funcall function
                                     ;; initialize
                                     (lambda ())
                                     ;; finalize
                                     (lambda (report)
                                       (declare (ignore report))
                                       (sdl2:push-quit-event)))))
                      (declare (ignore editor-thread))
                      nil))))

(defmethod lem-if:get-background-color ((implementation sdl2))
  (with-debug ("lem-if:get-background-color")
    (display-background-color *display*)))

(defmethod lem-if:update-foreground ((implementation sdl2) color)
  (with-debug ("lem-if:update-foreground" color)
    (setf (display-background-color *display*) color)
    ;; TODO: redraw
    ))

(defmethod lem-if:update-background ((implementation sdl2) color)
  (with-debug ("lem-if:update-background" color)
    (setf (display-foreground-color *display*) color)
    ;; TODO: redraw
    ))

(defmethod lem-if:display-width ((implementation sdl2))
  (with-debug ("lem-if:display-width")
    (floor (display-width *display*) (char-width))))

(defmethod lem-if:display-height ((implementation sdl2))
  (with-debug ("lem-if:display-height")
    (floor (display-height *display*) (char-height))))

(defmethod lem-if:make-view ((implementation sdl2) window x y width height use-modeline)
  (with-debug ("lem-if:make-view" window x y width height use-modeline)
    (make-view :window window :x x :y y :width width :height height :use-modeline use-modeline)))

(defmethod lem-if:delete-view ((implementation sdl2) view)
  (with-debug ("lem-if:delete-view")
    nil))

(defmethod lem-if:clear ((implementation sdl2) view)
  (with-debug ("lem-if:clear" view)
    (with-renderer ()
      (fill-rect (view-x view)
                 (view-y view)
                 (view-width view)
                 (view-height view)
                 :color (display-background-color *display*)))))

(defmethod lem-if:set-view-size ((implementation sdl2) view width height)
  (with-debug ("lem-if:set-view-size" view width height)
    (setf (view-width view) width
          (view-height view) height)))

(defmethod lem-if:set-view-pos ((implementation sdl2) view x y)
  (with-debug ("lem-if:set-view-pos" view x y)
    (setf (view-x view) x
          (view-y view) y)))

(defun attribute-foreground-color (attribute)
  (or (and attribute
           (lem:parse-color (lem:attribute-foreground attribute)))
      (display-foreground-color *display*)))

(defun attribute-background-color (attribute)
  (or (and attribute
           (lem:parse-color (lem:attribute-background attribute)))
      (display-background-color *display*)))

(defmethod lem-if:print ((implementation sdl2) view x y string attribute-or-name)
  (with-debug ("lem-if:print" view x y string attribute-or-name)
    (let ((attribute (lem:ensure-attribute attribute-or-name nil)))
      (with-renderer ()
        (render-fill-text string
                          (+ x (view-x view))
                          (+ y (view-y view))
                          :attribute attribute)))))

(defmethod lem-if:print-modeline ((implementation sdl2) view x y string attribute-or-name)
  (with-debug ("lem-if:print-modeline" view x y string attribute-or-name)
    (let ((attribute (lem:ensure-attribute attribute-or-name nil)))
      (with-renderer ()
        (render-fill-text string
                          (+ x (view-x view))
                          (+ y (view-y view) (view-height view))
                          :attribute attribute)))))

(defmethod lem-if:clear-eol ((implementation sdl2) view x y)
  (with-debug ("lem-if:clear-eol" view x y)
    (with-renderer ()
      (fill-rect (+ (view-x view) x)
                 (+ (view-y view) y)
                 (- (view-width view) x)
                 1
                 :color (display-background-color *display*)))))

(defmethod lem-if:clear-eob ((implementation sdl2) view x y)
  (with-debug ("lem-if:clear-eob" view x y)
    (with-renderer ()
      (fill-rect (+ (view-x view) x)
                 (+ (view-y view) y)
                 (- (view-width view) x)
                 1
                 :color (display-background-color *display*))
      (fill-rect (view-x view)
                 (+ (view-y view) y 1)
                 (view-width view)
                 (- (view-height view) y 1)
                 :color (display-background-color *display*)))))

(defun border-exists-p (window)
  (and (lem:floating-window-p window)
       (lem:floating-window-border window)
       (< 0 (lem:floating-window-border window))))

(defun draw-border (view)
  (when (border-exists-p (view-window view))
    (render-border (view-x view)
                   (view-y view)
                   (view-width view)
                   (view-height view))))

(defun draw-leftside-border (view)
  (when (and (< 0 (view-x view))
             (lem::window-use-modeline-p (view-window view)))

    (let ((attribute (lem:ensure-attribute 'lem:modeline-inactive)))
      (fill-rect (1- (view-x view))
                 (view-y view)
                 1
                 (1+ (view-height view))
                 :color (attribute-background-color attribute))

      (fill-rect-by-pixels (+ (* (1- (view-x view)) (char-width))
                              (floor (char-width) 2)
                              -1)
                           (* (view-y view) (char-height))
                           2
                           (* (+ (view-y view) (view-height view)) (char-height))
                           :color (attribute-foreground-color attribute)))))

(defmethod lem-if:redraw-view-after ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-after" view)
    (with-renderer ()
      (draw-border view)
      (draw-leftside-border view))))

(defmethod lem-if::will-update-display ((implementation sdl2))
  (with-renderer ()
    (sdl2:set-render-target (display-renderer *display*) (display-texture *display*))))

(defmethod lem-if:update-display ((implementation sdl2))
  (with-debug ("lem-if:update-display")
    (with-renderer ()
      (sdl2:set-render-target (display-renderer *display*) nil)
      (sdl2:render-copy (display-renderer *display*) (display-texture *display*))
      (update-display *display*))))

(defmethod lem-if:scroll ((implementation sdl2) view n)
  (with-debug ("lem-if:scroll" view n)
    ))

(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (sdl2-ffi.functions:sdl-get-clipboard-text))

(defmethod lem-if:clipboard-copy ((implementation sdl2) text)
  (sdl2-ffi.functions:sdl-set-clipboard-text text))

(pushnew :lem-sdl2 *features*)
