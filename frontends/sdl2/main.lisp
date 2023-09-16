(defpackage :lem-sdl2
  (:use :cl
        :lem-sdl2/keyboard
        :lem-sdl2/font
        :lem-sdl2/icon
        :lem-sdl2/platform
        :lem-sdl2/resource)
  (:export :change-font
           :set-keyboard-layout
           :render
           :current-renderer)
  (:export :draw-line
           :draw-rectangle
           :draw-point
           :draw-points
           :draw-string
           :load-image
           :delete-image
           :draw-image
           :delete-drawable
           :clear-drawables))
(in-package :lem-sdl2)

(pushnew :lem-sdl2 *features*)
(setf (lem:variable-value 'lem:highlight-line :global) t)

(defconstant +display-width+ 100)
(defconstant +display-height+ 40)

(defvar +lem-x11-wm-class+ "Lem SDL2")

(defmacro with-bindings (bindings &body body)
  `(let ,bindings
     (let ((bt:*default-special-bindings*
             (list* ,@(loop :for (var) :in bindings
                            :collect `(cons ',var ,var))
                    bt:*default-special-bindings*)))
       ,@body)))

(defun do-log (value)
  (let ((log-file (merge-pathnames "error.log" (lem:lem-home))))
    (ensure-directories-exist log-file)
    (with-open-file (out log-file
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (uiop:println value out))))

(defun call-with-debug (log-function body-function)
  (funcall log-function)
  (handler-bind ((error (lambda (e)
                          (log:info "~A"
                                    (with-output-to-string (out)
                                      (format out "~A~%" e)
                                      (uiop:print-backtrace :condition e :stream out))))))
    (funcall body-function)))

(defmacro with-debug ((&rest args) &body body)
  `(call-with-debug (lambda () (log:debug ,@args))
                    (lambda () ,@body)))

;; this is SDL2 way
;; if the stable version of SDL is 3, set WM_CLASS is set via hint SDL_HINT_APP_ID
;;
;; cf.
;; - how SDL3 gets WM_CLASS:
;;     - https://github.com/libsdl-org/SDL/blob/d3f2de7f297d761a7dc5b0dda3c7b5d7bd49eac9/src/video/x11/SDL_x11window.c#L633C40-L633C40
;; - how to set WM_CLASS in here:
;;     - SDL_SetHint() function with key SDL_HINT_APP_ID
;;     - https://wiki.libsdl.org/SDL2/SDL_SetHint
;;     - https://github.com/libsdl-org/SDL/blob/d3f2de7f297d761a7dc5b0dda3c7b5d7bd49eac9/src/core/unix/SDL_appid.c#L63C45-L63C45
(defun set-x11-wm-class (classname)
  (setf (uiop:getenv "SDL_VIDEO_X11_WMCLASS") classname))

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
   :redraw-after-modifying-floating-window nil))

(defvar *display*)

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
  (font-latin-normal-font (display-font display)))

(defmethod display-latin-bold-font ((display display))
  (font-latin-bold-font (display-font display)))

(defmethod display-cjk-normal-font ((display display))
  (font-cjk-normal-font (display-font display)))

(defmethod display-cjk-bold-font ((display display))
  (font-cjk-bold-font (display-font display)))

(defmethod display-emoji-font ((display display))
  (font-emoji-font (display-font display)))

(defmethod display-braille-font ((display display))
  (font-braille-font (display-font display)))

(defmethod display-background-color ((display display))
  (or (lem:parse-color lem-if:*background-color-of-drawing-window*)
      (slot-value display 'background-color)))

(defun char-width () (display-char-width *display*))
(defun char-height () (display-char-height *display*))
(defun current-renderer () (display-renderer *display*))

(defun call-with-renderer (function)
  (sdl2:in-main-thread ()
    (bt:with-recursive-lock-held ((display-mutex *display*))
      (funcall function))))

(defmacro with-renderer (() &body body)
  `(call-with-renderer (lambda () ,@body)))

(defmethod clear ((display display))
  (sdl2:set-render-target (display-renderer display) (display-texture display))
  (set-render-color display (display-background-color display))
  (sdl2:render-fill-rect (display-renderer display) nil))

(defvar *font-cache* (make-hash-table :test 'eql))

(defun clear-font-cache ()
  (clrhash *font-cache*))

(defun icon-font-name (character)
  (lem:icon-value (char-code character) :font))

(defun icon-font (character)
  (or (gethash character *font-cache*)
      (let ((font-name (icon-font-name character)))
        (when font-name
          (let ((pathname (get-resource-pathname (merge-pathnames font-name "resources/fonts/"))))
            (setf (gethash character *font-cache*)
                  (sdl2-ttf:open-font pathname
                                      (font-config-size (display-font-config *display*)))))))))

(defmethod get-display-font ((display display) &key type bold character)
  (check-type type (member :latin :cjk :braille :emoji :icon))
  (cond ((eq type :icon)
         (or (and character (icon-font character))
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
    (floor (* scale-x x) (char-width))))

(defmethod scaled-char-height ((display display) y)
  (let ((scale-y (round (second (display-scale display)))))
    (floor (* scale-y y) (char-height))))

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
          (create-texture (display-renderer display)
                          (display-width display)
                          (display-height display)))))

(defmethod set-render-color ((display display) color)
  (sdl2:set-render-draw-color (display-renderer display)
                              (lem:color-red color)
                              (lem:color-green color)
                              (lem:color-blue color)
                              0))

(defun notify-required-redisplay ()
  (with-renderer ()
    (when (display-redraw-at-least-once-p *display*)
      (setf (display-redraw-at-least-once-p *display*) nil)
      (sdl2:set-render-target (current-renderer) (display-texture *display*))
      (set-render-color *display* (display-background-color *display*))
      (sdl2:render-clear (current-renderer))
      #+darwin
      (adapt-high-dpi-display-scale)
      #+darwin
      (adapt-high-dpi-font-size)
      (lem:update-on-display-resized))))

(defun attribute-foreground-color (attribute)
  (or (and attribute
           (lem:parse-color (lem:attribute-foreground attribute)))
      (display-foreground-color *display*)))

(defun attribute-background-color (attribute)
  (or (and attribute
           (lem:parse-color (lem:attribute-background attribute)))
      (display-background-color *display*)))

(defun render-fill-rect-to-current-texture (x y width height &key color)
  (let ((x (* x (char-width)))
        (y (* y (char-height)))
        (width (* width (char-width)))
        (height (* height (char-height))))
    (sdl2:with-rects ((rect x y width height))
      (set-render-color *display* color)
      (sdl2:render-fill-rect (current-renderer) rect))))

(defun render-fill-rect (texture x y width height
                         &key (color (alexandria:required-argument :color)))
  (sdl2:set-render-target (current-renderer) texture)
  (render-fill-rect-to-current-texture x y width height :color color))

(defun render-line (x1 y1 x2 y2 &key color)
  (set-render-color *display* color)
  (sdl2:render-draw-line (current-renderer) x1 y1 x2 y2))

(defun render-texture (renderer texture x y width height)
  (sdl2:with-rects ((dest-rect x y width height))
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect nil
                         :dest-rect dest-rect
                         :flip (list :none))))

(defun cjk-char-code-p (display code)
  (and (typep code '(UNSIGNED-BYTE 16))
       (not (eql 0
                 (sdl2-ffi.functions:ttf-glyph-is-provided (display-cjk-normal-font display)
                                                           code)))))

(defun latin-char-code-p (display code)
  (and (typep code '(UNSIGNED-BYTE 16))
       (not (eql 0
                 (sdl2-ffi.functions:ttf-glyph-is-provided (display-latin-font display)
                                                           code)))))

(defun emoji-char-code-p (display code)
  (and (typep code '(UNSIGNED-BYTE 16))
       (not (eql 0 (sdl2-ffi.functions:ttf-glyph-is-provided (display-emoji-font display) code)))))

(defun braille-char-code-p (code)
  (<= #x2800 code #x28ff))

(defun icon-char-code-p (code)
  (icon-font-name (code-char code)))

(defun render-icon (character x y &key color)
  (cffi:with-foreign-string (c-string (string character))
    (let* ((x (* x (char-width)))
           (y (* y (char-height)))
           (surface (sdl2-ttf:render-utf8-blended
                     (get-display-font *display*
                                       :type :icon
                                       :character character)
                     c-string
                     (lem:color-red color)
                     (lem:color-green color)
                     (lem:color-blue color)
                     0))
           (text-width (sdl2:surface-width surface))
           (text-height (sdl2:surface-height surface))
           (texture (sdl2:create-texture-from-surface (current-renderer) surface))
           (offset-x (lem:icon-value (char-code character) :offset-x)))
      (render-texture (current-renderer)
                      texture
                      (if offset-x
                          (floor (+ x (* text-width offset-x)))
                          x)
                      y
                      text-width
                      text-height)
      (sdl2:destroy-texture texture)
      2)))

(defun render-folder-icon (x y)
  (let* ((image (sdl2-image:load-image (get-resource-pathname "resources/open-folder.png")))
         (texture (sdl2:create-texture-from-surface (current-renderer)
                                                    image)))
    (sdl2:with-rects ((dest-rect (* x (char-width))
                                 (* y (char-height))
                                 (* 2 (char-width))
                                 (floor (* (char-height) 0.9))))
      (sdl2:render-copy (current-renderer)
                        texture
                        :dest-rect dest-rect))
    (sdl2:free-surface image)
    (sdl2:destroy-texture texture)))

(defun guess-font-type (display code)
  (cond #+windows
        ((eql code #x1f4c1)
         ;; sdl2_ttf.dllでなぜか絵文字を表示できないので代わりにフォルダの画像を使う
         :folder)
        ((<= code 128)
         :latin)
        ((icon-char-code-p code)
         :icon)
        ((braille-char-code-p code)
         :braille)
        ((cjk-char-code-p display code)
         :cjk)
        ((latin-char-code-p display code)
         :latin)
        ((emoji-char-code-p display code)
         :emoji)
        (t
         :emoji)))

(defun render-character (character x y &key color bold)
  (handler-case
      (let* ((code (char-code character))
             (type (guess-font-type *display* code)))
        (case type
          (:folder
           (render-folder-icon x y)
           2)
          (:icon
           (render-icon character x y :color color))
          (otherwise
           (cffi:with-foreign-string (c-string (string character))
             (let* ((x (* x (char-width)))
                    (y (* y (char-height)))
                    (surface (sdl2-ttf:render-utf8-blended
                              (get-display-font *display*
                                                :type type
                                                :bold bold
                                                :character character)
                              c-string
                              (lem:color-red color)
                              (lem:color-green color)
                              (lem:color-blue color)
                              0))
                    (text-width (cond ((eq type :emoji)
                                       (* 2 (char-width)))
                                      ((eq type :braille)
                                       (char-width))
                                      (t
                                       (sdl2:surface-width surface))))
                    (text-height (cond ((eq type :emoji)
                                        (char-height))
                                       ((eq type :braille)
                                        (char-height))
                                       (t
                                        (sdl2:surface-height surface))))
                    (texture (sdl2:create-texture-from-surface (current-renderer) surface)))
               (render-texture (current-renderer) texture x y text-width text-height)
               (sdl2:destroy-texture texture)
               (if (member type '(:latin :braille)) 1 2))))))
    (sdl2-ttf::sdl-ttf-error ()
      (log:error "invalid character" character)
      1)))

(defun render-string (string x y &key color bold)
  (cffi:with-foreign-string (c-string string)
    (let* ((x (* x (char-width)))
           (y (* y (char-height)))
           (surface (sdl2-ttf:render-utf8-blended (get-display-font *display* :type :latin :bold bold)
                                                  c-string
                                                  (lem:color-red color)
                                                  (lem:color-green color)
                                                  (lem:color-blue color)
                                                  0))
           (height (sdl2:surface-height surface))
           (texture (sdl2:create-texture-from-surface (current-renderer) surface)))
      (render-texture (current-renderer)
                      texture
                      x
                      y
                      (* (char-width) (length string))
                      height)
      (sdl2:destroy-texture texture)
      (length string))))

(defun ascii-string-p (string)
  (every (lambda (c) (< (char-code c) 128)) string))

(defun render-text (text x y &key color bold)
  (unless (= (length text) 0)
    (if (ascii-string-p text)
        (render-string text x y :color color :bold bold)
        (loop :for c :across text
              :do (let ((offset (render-character c x y :color color :bold bold)))
                    (incf x offset))))))

(defun render-underline (x y width &key color)
  (render-line (* x (char-width))
               (- (* (1+ y) (char-height)) 1)
               (* (+ x width) (char-width))
               (- (* (1+ y) (char-height)) 1)
               :color color))

(defun render-fill-text (texture text x y &key attribute)
  (sdl2:set-render-target (current-renderer) texture)
  (let ((width (lem:string-width text))
        (underline (and attribute (lem:attribute-underline attribute)))
        (bold (and attribute (lem:attribute-bold attribute)))
        (reverse (and attribute (lem:attribute-reverse attribute))))
    (let ((background-color (if reverse
                                (attribute-foreground-color attribute)
                                (attribute-background-color attribute)))
          (foreground-color (if reverse
                                (attribute-background-color attribute)
                                (attribute-foreground-color attribute))))
      (render-fill-rect-to-current-texture x y width 1 :color background-color)
      (render-text text x y :color foreground-color :bold bold)
      (when underline
        (render-underline x
                          y
                          width
                          :color (if (eq underline t)
                                     foreground-color
                                     (or (lem:parse-color underline)
                                         foreground-color)))))))

(defun render-fill-rect-by-pixels (x y width height &key color)
  (sdl2:with-rects ((rect x y width height))
    (set-render-color *display* color)
    (sdl2:render-fill-rect (current-renderer) rect)))

(defun render-border (x y w h &key without-topline)
  (let* ((x1 (- (* x (char-width)) (floor (char-width) 2)))
         (y1 (- (* y (char-height)) (floor (char-height) 2)))
         (x2 (1- (+ x1 (* (+ w 1) (char-width)))))
         (y2 (+ y1 (* (+ h 1) (char-height)))))
    (sdl2:with-rects ((rect x1 y1 (- x2 x1) (- y2 y1)))
      (set-render-color *display* (display-background-color *display*))
      (sdl2:render-fill-rect (current-renderer) rect))

    (sdl2:with-points ((upleft x1 y1)
                       (downleft x1 y2)
                       (downright x2 y2)
                       (upright x2 y1))
      (if without-topline
          (progn
            (set-render-color *display* (display-foreground-color *display*))
            (sdl2:render-draw-lines (current-renderer) (sdl2:points* downleft upleft) 2)
            (set-render-color *display* (display-foreground-color *display*))
            (sdl2:render-draw-lines (current-renderer) (sdl2:points* upleft upright) 2))
          (progn
            (set-render-color *display* (display-foreground-color *display*))
            (sdl2:render-draw-lines (current-renderer) (sdl2:points* downleft upleft upright) 3)))
      (set-render-color *display* (display-foreground-color *display*))
      (sdl2:render-draw-lines (current-renderer) (sdl2:points* upright downright downleft) 3)

      ;; shadow
      #+(or)
      (sdl2:with-points ((downleft x1 (+ y2 2))
                         (downright (1+ x2) (+ y2 2))
                         (upright (+ x2 2) y1))
        (set-render-color *display* (lem:parse-color "black"))
        (sdl2:render-draw-lines (current-renderer) (sdl2:points* upright downright downleft) 3)))
    ))

(defun render-margin-line (x y height)
  (let ((attribute (lem:ensure-attribute 'lem:modeline-inactive)))
    (render-fill-rect-to-current-texture (1- x)
                      y
                      1
                      height
                      :color (attribute-background-color attribute))
    (render-fill-rect-by-pixels (+ (* (1- x) (char-width))
                                   (floor (char-width) 2)
                                   -1)
                                (* y (char-height))
                                2
                                (* height (char-height))
                                :color (attribute-foreground-color attribute))))

(defun change-font (font-config &optional (save-font-size-p t))
  (let ((font-config (merge-font-config font-config (display-font-config *display*))))
    (close-font (display-font *display*))
    (let ((font (open-font font-config)))
      (setf (display-char-width *display*) (font-char-width font)
            (display-char-height *display*) (font-char-height font))
      (setf (display-font-config *display*) font-config)
      (setf (display-font *display*) font))
    (when save-font-size-p
      (save-font-size font-config (first (display-scale *display*))))
    (clear-font-cache)
    (lem:send-event :resize)))

(defun create-view-texture (width height)
  (create-texture (current-renderer)
                  (* width (char-width))
                  (* height (char-height))))

(defclass view ()
  ((window
    :initarg :window
    :reader view-window)
   (x
    :initarg :x
    :accessor view-x)
   (y
    :initarg :y
    :accessor view-y)
   (width
    :initarg :width
    :accessor view-width)
   (height
    :initarg :height
    :accessor view-height)
   (use-modeline
    :initarg :use-modeline
    :reader view-use-modeline)
   (texture
    :initarg :texture
    :accessor view-texture)
   (last-cursor-x
    :initform nil
    :accessor view-last-cursor-x)
   (last-cursor-y
    :initform nil
    :accessor view-last-cursor-y)))

(defmethod last-cursor-x ((view view))
  (or (view-last-cursor-x view)
      ;; fallback to v1
      (* (lem:last-print-cursor-x (view-window view))
         (char-width))))

(defmethod last-cursor-y ((view view))
  (or (view-last-cursor-y view)
      ;; fallback to v1
      (* (lem:last-print-cursor-y (view-window view))
         (char-height))))

(defun create-view (window x y width height use-modeline)
  (when use-modeline (incf height))
  (make-instance 'view
                 :window window
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline use-modeline
                 :texture (create-view-texture width height)))

(defmethod delete-view ((view view))
  (when (view-texture view)
    (sdl2:destroy-texture (view-texture view))
    (setf (view-texture view) nil)))

(defmethod render-clear ((view view))
  (sdl2:set-render-target (current-renderer) (view-texture view))
  (set-render-color *display* (display-background-color *display*))
  (sdl2:render-clear (current-renderer)))

(defmethod resize ((view view) width height)
  (when (view-use-modeline view) (incf height))
  (setf (view-width view) width
        (view-height view) height)
  (sdl2:destroy-texture (view-texture view))
  (setf (view-texture view)
        (create-view-texture width height)))

(defmethod move-position ((view view) x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod render-text-using-view ((view view) x y string attribute)
  (render-fill-text (view-texture view)
                    string
                    x
                    y
                    :attribute attribute))

(defmethod render-text-to-modeline-using-view ((view view) x y string attribute)
  (render-fill-text (view-texture view)
                    string
                    x
                    (+ (view-height view) y -1)
                    :attribute attribute))

(defmethod draw-window-border (view (window lem:floating-window))
  (when (and (lem:floating-window-border window)
             (< 0 (lem:floating-window-border window)))
    (sdl2:set-render-target (current-renderer) (display-texture *display*))
    (render-border (lem:window-x window)
                   (lem:window-y window)
                   (lem:window-width window)
                   (lem:window-height window)
                   :without-topline (eq :drop-curtain (lem:floating-window-border-shape window)))))

(defmethod draw-window-border (view (window lem:window))
  (when (< 0 (lem:window-x window))
    (sdl2:set-render-target (current-renderer) (display-texture *display*))
    (render-margin-line (lem:window-x window)
                        (lem:window-y window)
                        (lem:window-height window))))

(defmethod render-border-using-view ((view view))
  (draw-window-border view (view-window view)))

(defmethod clear-eol ((view view) x y)
  (render-fill-rect (view-texture view)
                    x
                    y
                    (- (view-width view) x)
                    1
                    :color (display-background-color *display*)))

(defmethod clear-eob ((view view) x y)
  (clear-eol view x y)
  (render-fill-rect (view-texture view)
                    0
                    (+ y 1)
                    (view-width view)
                    (- (view-height view) y (if (view-use-modeline view) 2 1))
                    :color (display-background-color *display*)))

(defvar *cursor-shown* t)
(defun show-cursor ()
  (setf *cursor-shown* t)
  (sdl2:show-cursor))
(defun hide-cursor ()
  (setf *cursor-shown* nil)
  (sdl2:hide-cursor))

(defun on-mouse-button-down (button x y clicks)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4))))
    (when button
      (let ((char-x (scaled-char-width *display* x))
            (char-y (scaled-char-height *display* y)))
        (lem:send-event
          (lambda ()
            (lem:receive-mouse-button-down char-x char-y x y button
                                           clicks)))))))

(defun on-mouse-button-up (button x y)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4)))
        (char-x (scaled-char-width *display* x))
        (char-y (scaled-char-height *display* y)))
    (lem:send-event
     (lambda ()
       (lem:receive-mouse-button-up char-x char-y x y button)))))

(defun on-mouse-motion (x y state)
  (show-cursor)
  (let ((button (if (= sdl2-ffi:+sdl-button-lmask+ (logand state sdl2-ffi:+sdl-button-lmask+))
                    :button-1
                    nil)))
    (let ((char-x (scaled-char-width *display* x))
          (char-y (scaled-char-height *display* y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-motion char-x char-y x y button))))))

(defun on-mouse-wheel (wheel-x wheel-y which direction)
  (declare (ignore which direction))
  (show-cursor)
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (let ((char-x (scaled-char-width *display* x))
          (char-y (scaled-char-height *display* y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-wheel char-x char-y x y wheel-x wheel-y)
         (when (= 0 (lem:event-queue-length))
           (lem:redraw-display)))))))

(defun on-textediting (text)
  (handle-textediting (get-platform) text)
  (lem:send-event #'lem:redraw-display))

(defun on-textinput (value)
  (hide-cursor)
  (let ((text (etypecase value
                (integer (string (code-char value)))
                (string value))))
    (handle-text-input (get-platform) text)))

(defun on-keydown (key-event)
  (hide-cursor)
  (handle-key-down (get-platform) key-event))

(defun on-keyup (key-event)
  (handle-key-up (get-platform) key-event))

(defun on-windowevent (event)
  (alexandria:switch (event)
    (sdl2-ffi:+sdl-windowevent-shown+
     (notify-required-redisplay))
    (sdl2-ffi:+sdl-windowevent-exposed+
     (notify-required-redisplay))
    (sdl2-ffi:+sdl-windowevent-resized+
     (update-texture *display*)
     (notify-required-redisplay))
    (sdl2-ffi:+sdl-windowevent-focus-gained+
     (setf (display-focus-p *display*) t))
    (sdl2-ffi:+sdl-windowevent-focus-lost+
     (setf (display-focus-p *display*) nil))))

(defun event-loop ()
  (sdl2:with-event-loop (:method :wait)
    (:quit ()
     #+windows
     (cffi:foreign-funcall "_exit")
     t)
    (:textinput (:text text)
     (on-textinput text))
    (:textediting (:text text)
     (on-textediting text))
    (:keydown (:keysym keysym)
     (on-keydown (keysym-to-key-event keysym)))
    (:keyup (:keysym keysym)
     (on-keyup (keysym-to-key-event keysym)))
    (:mousebuttondown (:button button :x x :y y :clicks clicks)
     (on-mouse-button-down button x y clicks))
    (:mousebuttonup (:button button :x x :y y)
     (on-mouse-button-up button x y))
    (:mousemotion (:x x :y y :state state)
     (on-mouse-motion x y state))
    (:mousewheel (:x x :y y :which which :direction direction)
     (on-mouse-wheel x y which direction))
    (:windowevent (:event event)
     (on-windowevent event))))

(defun init-application-icon (window)
  (let ((image (sdl2-image:load-image (get-resource-pathname "resources/icon.png"))))
    (sdl2-ffi.functions:sdl-set-window-icon window image)
    (sdl2:free-surface image)))

(defun adapt-high-dpi-display-scale ()
  (with-debug ("adapt-high-dpi-display-scale")
    (with-renderer ()
      (multiple-value-bind (renderer-width renderer-height)
          (sdl2:get-renderer-output-size (current-renderer))
        (let* ((window-width (display-window-width *display*))
               (window-height (display-window-height *display*))
               (scale-x (/ renderer-width window-width))
               (scale-y (/ renderer-height window-height)))
          (setf (display-scale *display*) (list scale-x scale-y)))))))

(defun adapt-high-dpi-font-size ()
  (with-debug ("adapt-high-dpi-font-size")
    (with-renderer ()
      (let ((font-config (display-font-config *display*))
            (ratio (round (first (display-scale *display*)))))
        (change-font (change-size font-config
                                  (* ratio (lem:config :sdl2-font-size lem-sdl2/font::*default-font-size*)))
                     nil)))))

(defun create-display (function)
  (set-x11-wm-class +lem-x11-wm-class+)
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (sdl2-image:init '(:png))
    (unwind-protect
         (let* ((font-config (make-font-config))
                (font (open-font font-config))
                (char-width (font-char-width font))
                (char-height (font-char-height font)))
           (let ((window-width (* +display-width+ char-width))
                 (window-height (* +display-height+ char-height)))
             (sdl2:with-window (window :title "Lem"
                                       :w window-width
                                       :h window-height
                                       :flags '(:shown :resizable #+darwin :allow-highdpi))
               (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
                 (let* ((renderer-size (multiple-value-list (sdl2:get-renderer-output-size renderer)))
                        (renderer-width (first renderer-size))
                        (renderer-height (second renderer-size))
                        (scale-x #-darwin 1 #+darwin (/ renderer-width window-width))
                        (scale-y #-darwin 1 #+darwin (/ renderer-height window-height))
                        (texture (create-texture renderer
                                                 (* scale-x window-width)
                                                 (* scale-y window-height))))
                   (setf *display*
                         (make-instance
                          'display
                          :font-config font-config
                          :font font
                          :renderer renderer
                          :window window
                          :texture texture
                          :char-width (font-char-width font)
                          :char-height (font-char-height font)
                          :scale (list scale-x scale-y)))
                   (init-application-icon window)
                   #+darwin
                   (adapt-high-dpi-font-size)
                   (sdl2:start-text-input)
                   (funcall function)
                   (event-loop))))))
      (sdl2-ttf:quit)
      (sdl2-image:quit))))

(defun sbcl-on-darwin-p ()
  (or #+(and sbcl darwin)
      t
      nil))

(defmethod lem-if:invoke ((implementation sdl2) function)
  (flet ((thunk ()
           (let ((editor-thread
                   (funcall function
                            ;; initialize
                            (lambda ())
                            ;; finalize
                            (lambda (report)
                              (when report
                                (do-log report))
                              (sdl2:push-quit-event)))))
             (declare (ignore editor-thread))
             nil)))
    (progn
      ;; called *before* any sdl windows are created
      (sdl2:set-hint :video-mac-fullscreen-spaces
		     ;; the sdl2 library expects zero or one NOTE since this
		     ;; is a preference let's not change the default here
		     ;; because it's easy enough to change it via a user's
		     ;; config
		     (if (lem:config :darwin-use-native-fullscreen) 1 0))
      (sdl2:make-this-thread-main (lambda ()
				    (create-display #'thunk)
				    (when (sbcl-on-darwin-p)
				      (cffi:foreign-funcall "_exit")))))))

(defmethod lem-if:get-background-color ((implementation sdl2))
  (with-debug ("lem-if:get-background-color")
    (display-background-color *display*)))

(defmethod lem-if:update-foreground ((implementation sdl2) color)
  (with-debug ("lem-if:update-foreground" color)
    (setf (display-foreground-color *display*) (lem:parse-color color))))

(defmethod lem-if:update-background ((implementation sdl2) color)
  (with-debug ("lem-if:update-background" color)
    (setf (display-background-color *display*) (lem:parse-color color))))

(defmethod lem-if:display-width ((implementation sdl2))
  (with-debug ("lem-if:display-width")
    (with-renderer ()
      (floor (display-width *display*) (char-width)))))

(defmethod lem-if:display-height ((implementation sdl2))
  (with-debug ("lem-if:display-height")
    (with-renderer ()
      (floor (display-height *display*) (char-height)))))

(defmethod lem-if:display-title ((implementation sdl2))
  (with-debug ("lem-if:display-title")
    (sdl2:get-window-title (display-window *display*))))

(defmethod lem-if:set-display-title ((implementation sdl2) title)
  (with-debug ("lem-if:set-display-title")
    (sdl2:in-main-thread ()
      (with-renderer ()
        (sdl2:set-window-title (display-window *display*) title)
        ;; return the title instead of nil
        title))))

(defmethod lem-if:display-fullscreen-p ((implementation sdl2))
  (with-debug ("lem-if:display-fullscreen-p")
    (not (null (member :fullscreen (sdl2:get-window-flags (display-window *display*)))))))

(defmethod lem-if:set-display-fullscreen-p ((implementation sdl2) fullscreen-p)
  (with-debug ("lem-if:set-display-fullscreen-p")
    (sdl2:in-main-thread ()
      (with-renderer ()
        ;; always send :desktop over :fullscreen due to weird bugs on macOS
        (sdl2:set-window-fullscreen (display-window *display*)
                                    (if fullscreen-p :desktop))))))

(defmethod lem-if:make-view ((implementation sdl2) window x y width height use-modeline)
  (with-debug ("lem-if:make-view" window x y width height use-modeline)
    (with-renderer ()
      (create-view window x y width height use-modeline))))

(defmethod lem-if:delete-view ((implementation sdl2) view)
  (with-debug ("lem-if:delete-view")
    (with-renderer ()
      (delete-view view))))

(defmethod lem-if:clear ((implementation sdl2) view)
  (with-debug ("lem-if:clear" view)
    (with-renderer ()
      (render-clear view))))

(defmethod lem-if:set-view-size ((implementation sdl2) view width height)
  (with-debug ("lem-if:set-view-size" view width height)
    (with-renderer ()
      (resize view width height))))

(defmethod lem-if:set-view-pos ((implementation sdl2) view x y)
  (with-debug ("lem-if:set-view-pos" view x y)
    (with-renderer ()
      (move-position view x y))))

(defmethod lem-if:print ((implementation sdl2) view x y string attribute-or-name)
  (with-debug ("lem-if:print" view x y string attribute-or-name)
    (with-renderer ()
      (let ((attribute (lem:ensure-attribute attribute-or-name nil)))
        (render-text-using-view view x y string attribute)))))

(defmethod lem-if:print-modeline ((implementation sdl2) view x y string attribute-or-name)
  (with-debug ("lem-if:print-modeline" view x y string attribute-or-name)
    (with-renderer ()
      (let ((attribute (lem:ensure-attribute attribute-or-name nil)))
        (render-text-to-modeline-using-view view x y string attribute)))))

(defmethod lem-if:clear-eol ((implementation sdl2) view x y)
  (with-debug ("lem-if:clear-eol" view x y)
    (with-renderer ()
      (clear-eol view x y))))

(defmethod lem-if:clear-eob ((implementation sdl2) view x y)
  (with-debug ("lem-if:clear-eob" view x y)
    (with-renderer ()
      (clear-eob view x y))))

(defmethod lem-if:redraw-view-before ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-before" view)
    (with-renderer ()
      (render-border-using-view view))))

(defun render-view-texture-to-display (view)
  (sdl2:set-render-target (current-renderer) (display-texture *display*))
  (sdl2:with-rects ((dest-rect (* (view-x view) (char-width))
                               (* (view-y view) (char-height))
                               (* (view-width view) (char-width))
                               (* (view-height view) (char-height))))
    (sdl2:render-copy (current-renderer)
                      (view-texture view)
                      :dest-rect dest-rect)))

(defgeneric render (texture window buffer))

(defmethod lem-if:redraw-view-after ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-after" view)
    (with-renderer ()
      (sdl2:with-rects ((view-rect 0
                                   0
                                   (* (view-width view) (char-width))
                                   (* (1- (view-height view)) (char-height))))
        (sdl2:render-set-viewport (current-renderer) view-rect)
        (render (view-texture view)
                (view-window view)
                (lem:window-buffer (view-window view)))
        (sdl2:render-set-viewport (current-renderer) nil))
      (render-view-texture-to-display view))))

(defmethod lem-if::will-update-display ((implementation sdl2))
  (with-debug ("will-update-display")
    (with-renderer ()
      (sdl2:set-render-target (current-renderer) (display-texture *display*))
      (set-color (display-background-color *display*))
      (sdl2:render-clear (current-renderer)))))

(defun set-input-method ()
  (let* ((view (lem:window-view (lem:current-window)))
         (cursor-x (last-cursor-x view))
         (cursor-y (last-cursor-y view))
         (text lem-sdl2/keyboard::*textediting-text*)
         (x (+ (* (view-x view) (char-width))
               cursor-x))
         (y (+ (* (view-y view) (char-height))
               cursor-y)))
    (sdl2:with-rects ((rect x y (* (char-width) (lem:string-width text)) (char-height)))
      (sdl2-ffi.functions:sdl-set-text-input-rect rect)
      (when (plusp (length text))
        (let* ((color (display-foreground-color *display*))
               (surface (sdl2-ttf:render-utf8-blended (display-cjk-normal-font *display*)
                                                      text
                                                      (lem:color-red color)
                                                      (lem:color-green color)
                                                      (lem:color-blue color)
                                                      0))
               (texture (sdl2:create-texture-from-surface (display-renderer *display*) surface)))
          (sdl2:with-rects ((rect x y (sdl2:surface-width surface) (sdl2:surface-height surface)))
            (set-render-color *display* (display-background-color *display*))
            (sdl2:render-fill-rect (display-renderer *display*) rect)
            (sdl2:render-copy (display-renderer *display*) texture :dest-rect rect))
          (sdl2:destroy-texture texture))))))

(defmethod lem-if:update-display ((implementation sdl2))
  (with-debug ("lem-if:update-display")
    (with-renderer ()
      (setf (display-redraw-at-least-once-p *display*) t)
      (sdl2:set-render-target (current-renderer) nil)
      (sdl2:render-copy (current-renderer) (display-texture *display*))
      (set-input-method)
      (update-display *display*))))

(defmethod lem-if:increase-font-size ((implementation sdl2))
  (with-debug ("increase-font-size")
    (with-renderer ()
      (let ((font-config (display-font-config *display*))
            (ratio (round (first (display-scale *display*)))))
        (change-font (change-size font-config
                                  (+ (font-config-size font-config) ratio)))))))

(defmethod lem-if:decrease-font-size ((implementation sdl2))
  (with-debug ("decrease-font-size")
    (with-renderer ()
      (let ((font-config (display-font-config *display*))
            (ratio (round (first (display-scale *display*)))))
        (change-font (change-size font-config
                                  (- (font-config-size font-config) ratio)))))))

(defmethod lem-if:resize-display-before ((implementation sdl2))
  (with-debug ("resize-display-before")
    (with-renderer ()
      (clear *display*))))

(defmethod lem-if:get-font-list ((implementation sdl2))
  (get-font-list (get-platform)))

(defmethod lem-if:get-mouse-position ((implementation sdl2))
  (if (not *cursor-shown*)
      (values 0 0)
      (multiple-value-bind (x y bitmask)
          (sdl2:mouse-state)
        (declare (ignore bitmask))
        (values (scaled-char-width *display* x)
                (scaled-char-height *display* y)))))

(defmethod lem-if:get-char-width ((implementation sdl2))
  (char-width))

(defmethod lem-if:get-char-height ((implementation sdl2))
  (char-height))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass drawable ()
  ((target :initarg :target
           :reader drawable-target)
   (draw-function :initarg :draw-function
                  :reader drawable-draw-function)
   (targets :initform '()
            :accessor drawable-targets)))

(defun buffer-drawables (buffer)
  (lem:buffer-value buffer 'drawables))

(defun (setf buffer-drawables) (drawables buffer)
  (setf (lem:buffer-value buffer 'drawables) drawables))

(defun window-drawables (window)
  (lem:window-parameter window 'drawables))

(defun (setf window-drawables) (drawables window)
  (setf (lem:window-parameter window 'drawables) drawables))

(defmethod drawables ((target lem:window))
  (window-drawables target))

(defmethod drawables ((target lem:buffer))
  (buffer-drawables target))

(defmethod (setf drawables) (drawables (target lem:window))
  (setf (window-drawables target) drawables))

(defmethod (setf drawables) (drawables (target lem:buffer))
  (setf (buffer-drawables target) drawables))

(defmethod add-drawable ((target lem:window) drawable)
  (push target (drawable-targets drawable))
  (push drawable (lem:window-parameter target 'drawables)))

(defmethod add-drawable ((target lem:buffer) drawable)
  (push target (drawable-targets drawable))
  (push drawable (lem:buffer-value target 'drawables)))

(defun delete-drawable (drawable)
  (dolist (target (drawable-targets drawable))
    (alexandria:deletef (drawables target) drawable)))

(defun clear-drawables (target)
  (mapc #'delete-drawable (drawables target))
  (values))

(defmethod render (texture window buffer)
  (dolist (drawable (window-drawables window))
    (funcall (drawable-draw-function drawable)))
  (dolist (drawable (buffer-drawables buffer))
    (funcall (drawable-draw-function drawable))))

(defun call-with-drawable (target draw-function)
  (let ((drawable
          (make-instance 'drawable
                         :target target
                         :draw-function draw-function)))
    (add-drawable target drawable)
    drawable))

(defmacro with-drawable ((target) &body body)
  `(call-with-drawable ,target (lambda () ,@body)))

(defun set-color (color)
  (when color
    (set-render-color *display* color)))

(defun draw-line (target x1 y1 x2 y2 &key color)
  (with-drawable (target)
    (set-color color)
    (sdl2:render-draw-line (current-renderer)
                           x1
                           y1
                           x2
                           y2)))

(defun draw-rectangle (target x y width height &key filled color)
  (with-drawable (target)
    (set-color color)
    (sdl2:with-rects ((rect x y width height))
      (if filled
          (sdl2:render-fill-rect (current-renderer) rect)
          (sdl2:render-draw-rect (current-renderer) rect)))))

(defun draw-point (target x y &key color)
  (with-drawable (target)
    (set-color color)
    (sdl2:render-draw-point (current-renderer) x y)))

(defun convert-to-points (x-y-seq)
  (let ((num-points (length x-y-seq)))
    (plus-c:c-let ((c-points sdl2-ffi:sdl-point :count num-points))
      (etypecase x-y-seq
        (vector
         (loop :for i :from 0
               :for (x . y) :across x-y-seq
               :do (let ((dest-point (c-points i)))
                     (sdl2::c-point (dest-point)
                       (setf (dest-point :x) x
                             (dest-point :y) y))))))
      (values (c-points plus-c:&)
              num-points))))

(defun draw-points (target x-y-seq &key color)
  (multiple-value-bind (points num-points)
      (convert-to-points x-y-seq)
    (with-drawable (target)
      (set-color color)
      (sdl2:render-draw-points (current-renderer)
                               points
                               num-points))))

(defun draw-string (target string x y
                    &key (font (font-latin-normal-font (display-font *display*)))
                         color)
  (let* ((surface (sdl2-ttf:render-utf8-blended font
                                                string
                                                (lem:color-red color)
                                                (lem:color-green color)
                                                (lem:color-blue color)
                                                0)))
    (with-drawable (target)
      (let ((texture (sdl2:create-texture-from-surface (current-renderer) surface)))
        (sdl2:with-rects ((dest-rect x
                                     y
                                     (sdl2:surface-width surface)
                                     (sdl2:surface-height surface)))
          (sdl2:render-copy (current-renderer) texture :dest-rect dest-rect))
        (sdl2:destroy-texture texture)))))

(defclass image ()
  ((texture :initarg :texture
            :reader image-texture
            :writer set-image-texture)
   (width :initarg :width
          :reader image-width)
   (height :initarg :height
           :reader image-height)))

(defun load-image (pathname)
  (let ((image (sdl2-image:load-image pathname)))
    (make-instance 'image
                   :width (sdl2:surface-width image)
                   :height (sdl2:surface-height image)
                   ;; TODO: memory leak
                   :texture (sdl2:create-texture-from-surface (current-renderer)
                                                              image))))

(defun delete-image (image)
  (sdl2:destroy-texture (image-texture image))
  (set-image-texture nil image))

(defun draw-image (target image
                   &key (x 0)
                        (y 0)
                        (width (image-width image))
                        (height (image-height image))
                        clip-rect)
  (with-drawable (target)
    (sdl2:with-rects ((dest-rect x y width height))
      (let ((source-rect
              (when clip-rect
                (destructuring-bind (x y w h) clip-rect
                  (sdl2:make-rect x y w h)))))
        (unwind-protect
             (sdl2:render-copy (current-renderer)
                               (image-texture image)
                               :source-rect source-rect
                               :dest-rect dest-rect)
          (when source-rect
            (sdl2:free-rect source-rect)))))))

;;;
(lem:enable-clipboard)

#-windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (with-debug ("clipboard-paste")
    (with-renderer ()
      (sdl2-ffi.functions:sdl-get-clipboard-text))))


#+windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (with-debug ("clipboard-paste")
    (with-renderer ()
      (with-output-to-string (out)
        (let ((text (sdl2-ffi.functions:sdl-get-clipboard-text)))
          (loop :for string :in (split-sequence:split-sequence #\newline text)
                :do (if (and (< 0 (length string))
                             (char= #\return (char string (1- (length string)))))
                        (write-line (subseq string 0 (1- (length string))) out)
                        (write-string string out))))))))

(defmethod lem-if:clipboard-copy ((implementation sdl2) text)
  (with-debug ("clipboard-copy")
    (with-renderer ()
      (sdl2-ffi.functions:sdl-set-clipboard-text text))))
