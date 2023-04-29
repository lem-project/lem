(defpackage :lem-sdl2
  (:use :cl
        :lem-sdl2/keyboard
        :lem-sdl2/font
        :lem-sdl2/icon
        :lem-sdl2/platform)
  (:export :change-font))
(in-package :lem-sdl2)

(defconstant +display-width+ 100)
(defconstant +display-height+ 40)

(defmacro with-bindings (bindings &body body)
  `(let ,bindings
     (let ((bt:*default-special-bindings*
             (list* ,@(loop :for (var) :in bindings
                            :collect `(cons ',var ,var))
                    bt:*default-special-bindings*)))
       ,@body)))

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

(defun create-texture (renderer width height)
  (sdl2:create-texture renderer
                       sdl2:+pixelformat-rgba8888+
                       sdl2-ffi:+sdl-textureaccess-target+
                       width
                       height))

(defun get-character-size (font)
  (let* ((surface (sdl2-ttf:render-text-solid font "A" 0 0 0 0))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (list width height)))

(defclass sdl2 (lem:implementation)
  ()
  (:default-initargs
   :name :sdl2
   :native-scroll-support nil ; TODO: t
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
   (required-redisplay :initform nil
                       :accessor display-required-redisplay-p)
   (required-redisplay-mutex :initform (bt:make-lock "display-required-redisplay-mutex")
                             :accessor display-required-redisplay-mutex)))

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

(defmethod display-background-color ((display display))
  (or (lem:parse-color lem-if:*background-color-of-drawing-window*)
      (slot-value display 'background-color)))

(defun char-width () (display-char-width *display*))
(defun char-height () (display-char-height *display*))
(defun current-renderer () (display-renderer *display*))

(defun call-with-renderer (function)
  (bt:with-recursive-lock-held ((display-mutex *display*))
    (funcall function)))

(defmacro with-renderer (() &body body)
  `(call-with-renderer (lambda () ,@body)))

(defmethod get-display-font ((display display) &key type bold)
  (check-type type (member :latin :cjk :emoji))
  (if (eq type :emoji)
      (display-emoji-font display)
      (if bold
          (if (eq type :latin)
              (display-latin-bold-font display)
              (display-cjk-bold-font display))
          (if (eq type :latin)
              (display-latin-font display)
              (display-cjk-normal-font display)))))

(defmethod update-display ((display display))
  (sdl2:render-present (display-renderer display)))

(defmethod display-width ((display display))
  (nth-value 0 (sdl2:get-window-size (display-window display))))

(defmethod display-height ((display display))
  (nth-value 1 (sdl2:get-window-size (display-window display))))

(defmethod update-texture ((display display))
  (bt:with-lock-held ((display-mutex display))
    (sdl2:destroy-texture (display-texture display))
    (setf (display-texture display)
          (create-texture (display-renderer display)
                          (display-width display)
                          (display-height display)))))

(defun apply-resize ()
  (when (and (display-required-redisplay-p *display*)
             (display-redraw-at-least-once-p *display*))
    (setf (display-required-redisplay-p *display*) nil)
    (with-renderer ()
      (sdl2:set-render-target (current-renderer) (display-texture *display*))
      (set-render-color *display* (display-background-color *display*))
      (sdl2:render-clear (current-renderer))
      (lem::change-display-size-hook))))

(defmethod set-render-color ((display display) color)
  (sdl2:set-render-draw-color (display-renderer display)
                              (lem:color-red color)
                              (lem:color-green color)
                              (lem:color-blue color)
                              0))

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
       (sdl2-ffi.functions:ttf-glyph-is-provided (display-cjk-normal-font display) code)))

(defun render-character (character x y &key color bold)
  (handler-case
      (let* ((code (char-code character))
             (type (cond ((<= code 128)
                          :latin)
                         ((cjk-char-code-p *display* code)
                          :cjk)
                         (t
                          :emoji))))
        (cffi:with-foreign-string (c-string (string character))
          (let* ((x (* x (char-width)))
                 (y (* y (char-height)))
                 (surface (sdl2-ttf:render-utf8-blended
                           (get-display-font *display*
                                             :type type
                                             :bold bold)
                           c-string
                           (lem:color-red color)
                           (lem:color-green color)
                           (lem:color-blue color)
                           0))
                 (text-width (if (eq type :emoji)
                                 (* 2 (char-width))
                                 (sdl2:surface-width surface)))
                 (text-height (if (eq type :emoji)
                                  (char-height)
                                  (sdl2:surface-height surface)))
                 (texture (sdl2:create-texture-from-surface (current-renderer) surface)))
            (render-texture (current-renderer) texture x y text-width text-height)
            (sdl2:destroy-texture texture)
            (if (eq type :latin) 1 2))))
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
           (width (sdl2:surface-width surface))
           (height (sdl2:surface-height surface))
           (texture (sdl2:create-texture-from-surface (current-renderer) surface)))
      (render-texture (current-renderer) texture x y width height)
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
        (underline (and attribute (lem:attribute-underline-p attribute)))
        (bold (and attribute (lem:attribute-bold-p attribute)))
        (reverse (and attribute (lem:attribute-reverse-p attribute))))
    (let ((background-color (if reverse
                                (attribute-foreground-color attribute)
                                (attribute-background-color attribute)))
          (foreground-color (if reverse
                                (attribute-background-color attribute)
                                (attribute-foreground-color attribute))))
      (render-fill-rect-to-current-texture x y width 1 :color background-color)
      (render-text text x y :color foreground-color :bold bold)
      (when underline
        (render-underline x y width :color foreground-color)))))

(defun render-fill-rect-by-pixels (x y width height &key color)
  (sdl2:with-rects ((rect x y width height))
    (set-render-color *display* color)
    (sdl2:render-fill-rect (current-renderer) rect)))

(defun render-border (x y w h &key without-topline)
  (let* ((x1 (- (* x (char-width)) (floor (char-width) 2)))
         (y1 (- (* y (char-height)) (floor (char-height) 2)))
         (x2 (1- (+ x1 (* (+ w 1) (char-width)))))
         (y2 (+ y1 (* (+ h 1) (char-height)))))
    (sdl2:with-rects ((up-rect x1
                               y1
                               (* (+ w 1) (char-width))
                               (floor (char-height) 2))
                      (left-rect x1
                                 y1
                                 (floor (char-width) 2)
                                 (* (+ h 1) (char-height)))
                      (right-rect (* (+ x w) (char-width))
                                  y1
                                  (floor (char-width) 2)
                                  (* (+ h 1) (char-height)))
                      (down-rect x1
                                 (* (+ y h) (char-height))
                                 (* (+ w 1) (char-width))
                                 (floor (char-height) 2)))

      (set-render-color *display* (display-background-color *display*))
      (sdl2:render-fill-rect (current-renderer) up-rect)
      (sdl2:render-fill-rect (current-renderer) down-rect)
      (sdl2:render-fill-rect (current-renderer) left-rect)
      (sdl2:render-fill-rect (current-renderer) right-rect)

      (set-render-color *display* (display-foreground-color *display*))
      (if without-topline
          (sdl2:with-points ((upleft x1 y1)
                             (downleft x1 y2)
                             (downright x2 y2)
                             (upright x2 y1))
            (let ((points (sdl2:points* upleft downleft downright upright)))
              (sdl2:render-draw-lines (current-renderer)
                                      points
                                      4)))
          (sdl2:with-rects ((border-rect x1
                                         y1
                                         (* (+ 1 w) (char-width))
                                         (* (+ 1 h) (char-height))))
            (sdl2:render-draw-rect (current-renderer) border-rect))))))

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

(defun notify-required-redisplay ()
  (bt:with-lock-held ((display-required-redisplay-mutex *display*))
    (setf (display-required-redisplay-p *display*) t)))

(defun change-font (font-config)
  (let ((display *display*))
    (let ((font-config (merge-font-config font-config (display-font-config display))))
      (close-font (display-font display))
      (let ((font (open-font font-config)))
        (setf (display-char-width display) (font-char-width font)
              (display-char-height display) (font-char-height font))
        (setf (display-font-config display) font-config)
        (setf (display-font display) font)))
    (notify-required-redisplay)))

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
    :accessor view-texture)))

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

(defun on-mouse-button-down (button x y clicks)
  (sdl2:show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2))))
    (when button
      (let ((x (floor x (char-width)))
            (y (floor y (char-height))))
        (lem:send-event (lambda ()
                          (lem::handle-mouse-button-down x y button clicks)
                          (lem:redraw-display)))))))

(defun on-mouse-button-up (button x y)
  (sdl2:show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2))))
    (lem:send-event (lambda ()
                      (lem::handle-mouse-button-up x y button)
                      (lem:redraw-display)))))

(defun on-mouse-motion (x y state)
  (sdl2:show-cursor)
  (when (= sdl2-ffi:+sdl-button-lmask+ (logand state sdl2-ffi:+sdl-button-lmask+))
    (let ((x (floor x (char-width)))
          (y (floor y (char-height))))
      (lem:send-event (lambda ()
                        (lem::handle-mouse-motion x y :button-1)
                        (when (= 0 (lem::event-queue-length))
                          (lem:redraw-display)))))))

(defun on-mouse-wheel (wheel-x wheel-y which direction)
  (declare (ignore which direction))
  (sdl2:show-cursor)
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (let ((x (floor x (char-width)))
          (y (floor y (char-height))))
      (lem:send-event (lambda ()
                        (lem::handle-mouse-wheel x y wheel-x wheel-y)
                        (when (= 0 (lem::event-queue-length))
                          (lem:redraw-display)))))))

(defun on-textediting (text)
  (handle-textediting (get-platform) text)
  (lem:send-event #'lem:redraw-display))

(defun on-textinput (text)
  (sdl2:hide-cursor)
  (handle-text-input (get-platform) text))

(defun on-keydown (key-event)
  (sdl2:hide-cursor)
  (handle-key-down (get-platform) key-event))

(defun on-keyup (key-event)
  (handle-key-up (get-platform) key-event))

(defun on-windowevent (event)
  (cond ((or (equal event
                    sdl2-ffi:+sdl-windowevent-shown+)
             (equal event
                    sdl2-ffi:+sdl-windowevent-exposed+))
         (notify-required-redisplay))
        ((equal event sdl2-ffi:+sdl-windowevent-resized+)
         (update-texture *display*)
         (notify-required-redisplay))
        ((equal event sdl2-ffi:+sdl-windowevent-focus-gained+)
         (setf (display-focus-p *display*) t))
        ((equal event sdl2-ffi:+sdl-windowevent-focus-lost+)
         (setf (display-focus-p *display*) nil))))

(defun convert-event (event)
  (let ((event-type (sdl2:get-event-type event)))
    (case event-type
      (:quit
       (list event-type))
      (:textinput
       (let ((text (plus-c:c-ref event sdl2-ffi:sdl-event :text :text string)))
         (list event-type text)))
      (:textediting
       (let ((text (plus-c:c-ref event sdl2-ffi:sdl-event :edit :text string)))
         (list event-type text)))
      (:keydown
       (when (display-focus-p *display*)
         (let* ((keysym (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym))
                (code (sdl2:sym-value keysym))
                (modifier (lem-sdl2/keyboard::get-modifier keysym)))
           (list event-type (make-key-event code modifier)))))
      (:keyup
       (let* ((keysym (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym))
              (code (sdl2:sym-value keysym))
              (modifier (lem-sdl2/keyboard::get-modifier keysym)))
         (list event-type (make-key-event code modifier))))
      (:mousebuttondown
       (let ((button
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :button))
             (x
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :x))
             (y
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :y))
             (clicks
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :clicks)))
         (list event-type
               button
               x
               y
               clicks)))
      (:mousebuttonup
       (let ((button
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :button))
             (x
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :x))
             (y
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :button :y)))
         (list event-type button x y)))
      (:mousemotion
       (let ((x
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :motion :x))
             (y
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :motion :y))
             (state
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :motion :state)))
         (list event-type x y state)))
      (:mousewheel
       (let ((x
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :wheel :x))
             (y
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :wheel :y))
             (which
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :wheel :which))
             (direction
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :wheel :direction)))
         (list event-type x y which direction)))
      (:windowevent
       (let ((event
               (plus-c:c-ref event
                             sdl2-ffi:sdl-event
                             :window :event)))
         (list event-type event))))))

(defun next-events (event)
  (bt:with-lock-held ((display-required-redisplay-mutex *display*))
    (apply-resize)
    (sdl2:next-event event :wait)
    (let ((events '()))
      (alexandria:when-let (event (convert-event event))
        (push event events))
      (loop :for rc := (sdl2:next-event event :poll)
            :while (= rc 1)
            :do (alexandria:when-let (event (convert-event event))
                  (push event events)))
      (nreverse events))))

(defun event-loop ()
  (sdl2:in-main-thread ()
    (sdl2:with-sdl-event (event)
      (loop
        (let ((events (next-events event)))
          (loop :for (type . args) :in events
                :do (case type
                      (:quit
                       (return-from event-loop))
                      (:textinput
                       (destructuring-bind (text) args
                         (on-textinput text)))
                      (:textediting
                       (destructuring-bind (text) args
                         (on-textediting text)))
                      (:keydown
                       (destructuring-bind (key-event) args
                         (on-keydown key-event)))
                      (:keyup
                       (destructuring-bind (key-event) args
                         (on-keyup key-event)))
                      (:mousebuttondown
                       (destructuring-bind (button x y clicks) args
                         (on-mouse-button-down button x y
                                               clicks)))
                      (:mousebuttonup
                       (destructuring-bind (button x y) args
                         (on-mouse-button-up button x y)))
                      (:mousemotion
                       (destructuring-bind (x y state) args
                         (on-mouse-motion x y state)))
                      (:mousewheel
                       (destructuring-bind (x y which direction) args
                         (on-mouse-wheel x y which direction)))
                      (:windowevent
                       (destructuring-bind (event) args
                         (on-windowevent event))))))))))

(defun create-display (function)
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
                                       :flags '(:shown :resizable))
               (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
                 (let ((texture (create-texture renderer
                                                window-width
                                                window-height)))
                   (setf *display*
                         (make-instance
                          'display
                          :font-config font-config
                          :font font
                          :renderer renderer
                          :window window
                          :texture texture
                          :char-width (font-char-width font)
                          :char-height (font-char-height font)))
                   (init-application-icon window)
                   (sdl2:start-text-input)
                   (funcall function)
                   (event-loop))))))
      (sdl2-ttf:quit)
      (sdl2-image:quit))))

(defmethod lem-if:invoke ((implementation sdl2) function)
  (let ((thread (bt:make-thread
                 (lambda ()
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
                                       nil)))))))
    (bt:join-thread thread)))

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

(defmethod lem-if:redraw-view-after ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-after" view)
    (with-renderer ()
      (sdl2:set-render-target (current-renderer) (display-texture *display*))
      (sdl2:with-rects ((dest-rect (* (view-x view) (char-width))
                                   (* (view-y view) (char-height))
                                   (* (view-width view) (char-width))
                                   (* (view-height view) (char-height)))
                        (src-rect 0
                                  0
                                  (* (view-width view) (char-width))
                                  (* (view-height view) (char-height))))
        (sdl2:render-copy (current-renderer)
                          (view-texture view)
                          :dest-rect dest-rect
                          :source-rect src-rect))
      (render-border-using-view view))))

(defmethod lem-if::will-update-display ((implementation sdl2))
  (with-debug ("will-update-display")
    (with-renderer ()
      (sdl2:set-render-target (current-renderer) (display-texture *display*)))))

(defun set-input-method ()
  (let* ((view (lem:window-view (lem:current-window)))
         (cursor-x (lem:last-print-cursor-x (lem:current-window)))
         (cursor-y (lem:last-print-cursor-y (lem:current-window)))
         (text lem-sdl2/keyboard::*textediting-text*)
         (x (+ (* (view-x view) (char-width))
               (* cursor-x (char-width))))
         (y (+ (* (view-y view) (char-height))
               (* cursor-y (char-height)))))
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

(defmethod lem-if:scroll ((implementation sdl2) view n)
  (with-debug ("lem-if:scroll" view n)
    ))

(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (with-debug ("clipboard-paste")
    (with-renderer ()
      (sdl2-ffi.functions:sdl-get-clipboard-text))))

(defmethod lem-if:clipboard-copy ((implementation sdl2) text)
  (with-debug ("clipboard-copy")
    (with-renderer ()
      (sdl2-ffi.functions:sdl-set-clipboard-text text))))

(defmethod lem-if:increase-font-size ((implementation sdl2))
  (with-debug ("increase-font-size")
    (with-renderer ()
      (let ((font-config (display-font-config *display*)))
        (change-font (change-size font-config
                                  (1+ (font-config-size font-config))))))))

(defmethod lem-if:decrease-font-size ((implementation sdl2))
  (with-debug ("decrease-font-size")
    (with-renderer ()
      (let ((font-config (display-font-config *display*)))
        (change-font (change-size font-config
                                  (1- (font-config-size font-config))))))))

(pushnew :lem-sdl2 *features*)
