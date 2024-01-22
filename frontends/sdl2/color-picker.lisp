(defpackage :lem-sdl2/color-picker
  (:use :cl)
  (:export :make-color-picker-buffer))
(in-package :lem-sdl2/color-picker)

(defparameter +slider-height+ 500)
(defparameter +square-x+ 0)
(defparameter +square-y+ 60)
(defparameter +square-size+ 500)
(defparameter +slider-x+ 510)
(defparameter +slider-y+ 60)
(defparameter +slider-width+ 50)
(defparameter +color-text-x+ 0)
(defparameter +color-text-y+ 0)

(defclass color-picker-buffer (lem:text-buffer)
  ((slider-position
    :initform 0
    :accessor slider-position)
   (square-cursor-x
    :initform 0
    :accessor square-cursor-x)
   (square-cursor-y
    :initform 0
    :accessor square-cursor-y)
   (selected-color
    :initform nil
    :accessor selected-color)
   (callback
    :accessor callback)))

(defun render-vertical-slider (&key x y width height slider-position)
  (let* ((red 255.0)
         (green 0.0)
         (blue 0.0)
         (segmented (/ height 6))
         (num (/ 255 (/ height 6)))
         selected-color)
    (loop :for i :from 0 :below height
          :do (setf blue (if (<= i segmented) (+ blue num) blue))
              (setf red (if (and (>= i segmented) (< i (* 2 segmented))) (- red num) red))
              (setf green (if (and (>= i (* 2 segmented)) (< i (* 3 segmented))) (+ green num) green))

              (setf blue (if (and (>= i (* 3 segmented)) (< i (* 4 segmented))) (- blue num) blue))
              (setf red (if (and (>= i (* 4 segmented)) (< i (* 5 segmented))) (+ red num) red))
              (setf green (if (>= i (* 5 segmented)) (- green num) green))

              (setf red (if (> red 255.0) 255 red))
              (setf green (if (> green 255.0) 255 green))
              (setf blue (if (> blue 255.0) 255 blue))

              (setf red (if (< red 0) 0 red))
              (setf green (if (< green 0) 0 green))
              (setf blue (if (< blue 0) 0 blue))

              (let ((color (lem:make-color (round red)
                                           (round green)
                                           (round blue))))
                (when (= i slider-position)
                  (setf selected-color color))

                (sdl2:with-rects ((rect x (+ y i) width 1))
                  (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display) color)
                  (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))))

    (sdl2:with-rects ((rect x (1- (+ slider-position y)) width 3))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 0 0 0))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))
    (sdl2:with-rects ((rect x (+ slider-position y) width 1))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 255 255 255))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))

    selected-color))

(defun render-square (&key red green blue x y size cursor-x cursor-y)
  (let* ((xdifference-red (- 255.0 red))
         (xdifference-green (- 255.0 green))
         (xdifference-blue (- 255.0 blue))
         (xred-delta (if (= xdifference-red 0) 0 (/ xdifference-red size)))
         (xgreen-delta (if (= xdifference-green 0) 0 (/ xdifference-green size)))
         (xblue-delta (if (= xdifference-blue 0) 0 (/ xdifference-blue size)))
         (ydifference-red 255.0)
         (ydifference-green 255.0)
         (ydifference-blue 255.0)
         (yred-delta (/ ydifference-red size))
         (ygreen-delta (/ ydifference-green size))
         (yblue-delta (/ ydifference-blue size))
         (current-red 255.0)
         (current-green 255.0)
         (current-blue 255.0)
         selected-color)
    (dotimes (current-y size)
      (dotimes (current-x size)
        (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                           (lem:make-color (round current-red)
                                                           (round current-green)
                                                           (round current-blue)))
        (sdl2:render-draw-point (lem-sdl2:current-renderer) (+ current-x x) (+ current-y y))
        (decf current-red xred-delta)
        (decf current-green xgreen-delta)
        (decf current-blue xblue-delta)
        (setf current-red (if (> current-red 255.0) 255 current-red))
        (setf current-green (if (> current-green 255.0) 255 current-green))
        (setf current-blue (if (> current-blue 255.0) 255 current-blue))
        (setf current-red (if (< current-red 0) 0 current-red))
        (setf current-green (if (< current-green 0) 0 current-green))
        (setf current-blue (if (< current-blue 0) 0 current-blue))
        (when (and (= current-x cursor-x) (= current-y cursor-y))
          (setf selected-color
                (lem:make-color (round current-red)
                                (round current-green)
                                (round current-blue)))))
      (setf current-red (- 255.0 (* yred-delta (1+ current-y))))
      (setf current-green (- 255.0 (* ygreen-delta (1+ current-y))))
      (setf current-blue (- 255.0 (* yblue-delta (1+ current-y)))))

    (sdl2:with-rects ((rect x (+ +square-y+ (1- cursor-y)) size 3))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 0 0 0))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))
    (sdl2:with-rects ((rect x (+ +square-y+ cursor-y) size 1))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 255 255 255))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))

    (sdl2:with-rects ((rect (1- cursor-x) +square-y+ 3 size))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 0 0 0))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))
    (sdl2:with-rects ((rect cursor-x +square-y+ 1 size))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 255 255 255))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))

    selected-color))

(defun render-color-text (x y color)
  (sdl2:with-rects ((rect x y (+ +square-size+ 10 +slider-width+) 50))
    (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display) color)
    (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect)))

(defmethod lem-sdl2:render (texture window (buffer color-picker-buffer))
  (sdl2:set-render-target (lem-sdl2:current-renderer) texture)
  (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                     (lem:make-color 100 100 100))
  (sdl2:render-fill-rect (lem-sdl2:current-renderer) nil)
  (let ((color (render-vertical-slider :x +slider-x+
                                       :y +slider-y+
                                       :width +slider-width+
                                       :height +slider-height+
                                       :slider-position (slider-position buffer))))
    (let ((color (render-square :red (lem:color-red color)
                                :green (lem:color-green color)
                                :blue (lem:color-blue color)
                                :x +square-x+
                                :y +square-y+
                                :size +square-size+
                                :cursor-x (square-cursor-x buffer)
                                :cursor-y (square-cursor-y buffer))))
      (setf (selected-color buffer) color)
      (render-color-text +color-text-x+ +color-text-y+ color))))

(defun handle-mouse-position (mouse-event window buffer)
  (multiple-value-bind (x y)
      (lem-core::get-relative-mouse-coordinates-pixels mouse-event window)
    (cond ((and (<= +slider-x+ x (+ +slider-width+ +slider-x+))
                (<= +slider-y+ y (+ +slider-y+ +slider-height+))
                (eql :button-1 (lem-core::mouse-event-button mouse-event)))
           (setf (slider-position buffer) (- y +slider-y+)))
          ((and (<= +square-x+ x +square-size+)
                (<= +square-y+ y (+ +square-y+ +square-size+))
                (eql :button-1 (lem-core::mouse-event-button mouse-event)))
           (setf (square-cursor-x buffer) (- x +square-x+)
                 (square-cursor-y buffer) (- y +square-y+))))))

(defmethod lem-core::handle-mouse-button-down ((buffer color-picker-buffer) mouse-event &key window)
  (handle-mouse-position mouse-event window buffer))

(defmethod lem-core::handle-mouse-button-up ((buffer color-picker-buffer) mouse-event &key window)
  (declare (ignore window))
  (funcall (callback buffer) (selected-color buffer)))

(defmethod lem-core::handle-mouse-hover ((buffer color-picker-buffer) mouse-event &key window)
  (handle-mouse-position mouse-event window buffer))

(lem:define-minor-mode color-picker-mode
    (:name "Color Picker"
     :keymap *color-picker-keymap*))

(lem:define-key *color-picker-keymap* "q" 'color-picker-quit)

(lem:define-command color-picker-quit () ()
  (assert (lem:mode-active-p (lem:window-buffer (lem:current-window)) 'color-picker-mode))
  (lem:quit-window (lem:current-window) :kill-buffer t))

(defun make-color-picker-buffer (buffer-name &key callback)
  (let ((buffer (lem:make-buffer buffer-name)))
    (change-class buffer 'color-picker-buffer)
    (lem:change-buffer-mode buffer 'color-picker-mode)
    (setf (callback buffer) callback)
    buffer))

(defmethod lem-color-preview:invoke-color-picker ((frontend lem-sdl2/sdl2:sdl2) callback)
  (setf (lem:current-window)
        (lem:pop-to-buffer
         (lem-sdl2/color-picker::make-color-picker-buffer
          "*Color Picker*"
          :callback callback))))
