(defpackage :lem-sdl2/view
  (:use :cl))
(in-package :lem-sdl2/view)

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
         (lem-sdl2/display::display-char-width lem-sdl2/display::*display*))))

(defmethod last-cursor-y ((view view))
  (or (view-last-cursor-y view)
      ;; fallback to v1
      (* (lem:last-print-cursor-y (view-window view))
         (lem-sdl2/display::display-char-height lem-sdl2/display::*display*))))

(defun create-view (window x y width height use-modeline)
  (when use-modeline (incf height))
  (make-instance 'view
                 :window window
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline use-modeline
                 :texture (lem-sdl2/display::create-view-texture lem-sdl2/display::*display* width height)))

(defmethod delete-view ((view view))
  (when (view-texture view)
    (sdl2:destroy-texture (view-texture view))
    (setf (view-texture view) nil)))

(defmethod render-clear ((view view))
  (sdl2:set-render-target (lem-sdl2/display::display-renderer lem-sdl2/display::*display*) (view-texture view))
  (lem-sdl2/display::set-render-color lem-sdl2/display::*display* (lem-sdl2/display::display-background-color lem-sdl2/display::*display*))
  (sdl2:render-clear (lem-sdl2/display::display-renderer lem-sdl2/display::*display*)))

(defmethod resize ((view view) width height)
  (when (view-use-modeline view) (incf height))
  (setf (view-width view) width
        (view-height view) height)
  (sdl2:destroy-texture (view-texture view))
  (setf (view-texture view)
        (lem-sdl2/display::create-view-texture lem-sdl2/display::*display* width height)))

(defmethod move-position ((view view) x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod draw-window-border (view (window lem:floating-window))
  (when (and (lem:floating-window-border window)
             (< 0 (lem:floating-window-border window)))
    (sdl2:set-render-target (lem-sdl2/display::display-renderer lem-sdl2/display::*display*) (lem-sdl2/display::display-texture lem-sdl2/display::*display*))
    (lem-sdl2/display::render-border lem-sdl2/display::*display*
                                     (lem:window-x window)
                                     (lem:window-y window)
                                     (lem:window-width window)
                                     (lem:window-height window)
                                     :without-topline (eq :drop-curtain (lem:floating-window-border-shape window)))))

(defmethod draw-window-border (view (window lem:window))
  (when (< 0 (lem:window-x window))
    (sdl2:set-render-target (lem-sdl2/display::display-renderer lem-sdl2/display::*display*) (lem-sdl2/display::display-texture lem-sdl2/display::*display*))
    (lem-sdl2/display::render-margin-line lem-sdl2/display::*display*
                                          (lem:window-x window)
                                          (lem:window-y window)
                                          (lem:window-height window))))

(defmethod render-border-using-view ((view view))
  (draw-window-border view (view-window view)))
