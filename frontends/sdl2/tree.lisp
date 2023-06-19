(defpackage :lem-sdl2/tree
  (:use :cl :lem))
(in-package :lem-sdl2/tree)

(defclass tree-view-buffer (text-buffer)
  ((drawables :initarg :drawables
              :accessor tree-view-buffer-drawables)
   (scroll-y :initform 0
             :accessor tree-view-buffer-scroll-y)
   (margin-x :initarg :margin-x
             :initform 100
             :accessor tree-view-buffer-margin-x)
   (margin-y :initarg :margin-y
             :initform 50
             :accessor tree-view-buffer-margin-y)
   (width :accessor tree-view-buffer-width)
   (height :accessor tree-view-buffer-height)))

(defmethod tree-view-display-end ((buffer tree-view-buffer))
  (+ (tree-view-buffer-scroll-y buffer)
     (lem-sdl2::display-height lem-sdl2::*display*)))

(defmethod tree-view-scroll ((buffer tree-view-buffer) n)
  (incf (tree-view-buffer-scroll-y buffer) n)
  (let* ((height (* (1- (window-height (current-window)))
                    (lem-if:get-char-height (implementation))))
         (last-y (max 0 (- (tree-view-buffer-height buffer) height))))
    (cond ((< last-y
              (tree-view-buffer-scroll-y buffer))
           (setf (tree-view-buffer-scroll-y buffer)
                 last-y))
          ((< (tree-view-buffer-scroll-y (current-buffer)) 0)
           (setf (tree-view-buffer-scroll-y (current-buffer)) 0)))))

(defclass node ()
  ((value :initarg :value
          :reader node-value)
   (children :initarg :children
             :initform '()
             :reader node-children)
   (x :initform 0
      :accessor node-x)
   (y :initform 0
      :accessor node-y)
   (last-y :initform 0
           :accessor node-last-y)))

(defmethod print-object ((node node) stream)
  (loop :repeat (* 2 (or *print-level* 0)) :do (write-char #\space stream))
  (print-unreadable-object (node stream :type t)
    (let ((*print-level* (1+ (or *print-level* 0))))
      (format stream "~S x:~D y:~D" (node-value node) (node-x node) (node-y node))
      (when(node-children node)
        (format stream "~%~{~S~^~%~}" (node-children node))))))

(defun compute-position-with-rightward-extending (node)
  (labels ((recursive (node current-y depth)
             (setf (node-x node) depth)
             (cond ((null (node-children node))
                    (setf (node-y node) current-y)
                    (setf (node-last-y node) current-y)
                    (node-y node))
                   (t
                    (loop :for child :in (node-children node)
                          :for i := current-y :then (1+ child-y)
                          :for child-y := (recursive child i (1+ depth))
                          :sum (node-y child) :into sum-y
                          :finally (setf (node-y node)
                                         (/ sum-y (length (node-children node)))))
                    (setf (node-last-y node)
                          (node-last-y (car (last (node-children node)))))))))
    (recursive node 0 0)))

(defclass text-node ()
  ((surface :initarg :surface
            :reader text-node-surface)
   (x :initarg :x
      :reader text-node-x)
   (y :initarg :y
      :reader text-node-y)
   (width :initarg :width
          :reader text-node-width)
   (height :initarg :height
           :reader text-node-height)
   (value :initarg :value
          :reader text-node-value)))

(defclass line-edge ()
  ((x0 :initarg :x0
       :reader line-edge-x0)
   (y0 :initarg :y0
       :reader line-edge-y0)
   (x1 :initarg :x1
       :reader line-edge-x1)
   (y1 :initarg :y1
       :reader line-edge-y1)
   (color :initarg :color
          :reader line-edge-color)))

(defun compute-width (objects)
  (loop :for object :in objects
        :when (typep object 'text-node)
        :maximize (+ (text-node-x object)
                     (text-node-width object))))

(defun compute-height (objects)
  (loop :for object :in objects
        :when (typep object 'text-node)
        :maximize (+ (text-node-y object)
                     (text-node-height object))))

(defun draw (buffer node)
  (let ((drawables '())
        (font (sdl2-ttf:open-font
               (lem-sdl2/resource:get-resource-pathname
                "resources/fonts/NotoSansMono-Regular.ttf")
               (lem-sdl2/font:font-config-size (lem-sdl2::display-font-config lem-sdl2::*display*)))))
    (labels ((recursive (node current-x)
               (let ((y (round (* (tree-view-buffer-margin-y buffer) (node-y node)))))
                 (let* ((surface (sdl2-ttf:render-utf8-blended font
                                                               (princ-to-string (node-value node))
                                                               255
                                                               255
                                                               255
                                                               0))
                        (node-width (sdl2:surface-width surface))
                        (node-height (sdl2:surface-height surface)))
                   (push (make-instance 'text-node
                                        :surface surface
                                        :x current-x
                                        :y y
                                        :width node-width
                                        :height node-height
                                        :value (node-value node))
                         drawables)
                   (dolist (child (node-children node))
                     (multiple-value-bind (child-x child-y child-width child-height)
                         (recursive child (+ (tree-view-buffer-margin-x buffer)
                                             (+ current-x node-width)))
                       (declare (ignore child-width))
                       (push (make-instance 'line-edge
                                            :color (lem:make-color 255 255 255)
                                            :x0 (+ current-x (sdl2:surface-width surface))
                                            :y0 (+ y (round (sdl2:surface-height surface) 2))
                                            :x1 child-x
                                            :y1 (+ child-y (round child-height 2)))
                             drawables)))
                   (values current-x
                           y
                           (sdl2:surface-width surface)
                           (sdl2:surface-height surface))))))
      (recursive node 0)
      (setf (tree-view-buffer-drawables buffer) drawables)
      (setf (tree-view-buffer-width buffer) (compute-width drawables))
      (setf (tree-view-buffer-height buffer) (compute-height drawables))
      (values))))

(defmethod render ((text-node text-node) buffer)
  (when (<= (tree-view-buffer-scroll-y buffer)
            (text-node-y text-node)
            (+ (text-node-y text-node)
               (text-node-height text-node))
            (tree-view-display-end buffer))
    (let ((texture
            (sdl2:create-texture-from-surface
             (lem-sdl2:current-renderer)
             (text-node-surface text-node))))
      (sdl2:with-rects ((dest-rect (text-node-x text-node)
                                   (- (text-node-y text-node)
                                      (tree-view-buffer-scroll-y buffer))
                                   (text-node-width text-node)
                                   (text-node-height text-node)))
        (sdl2:render-copy (lem-sdl2:current-renderer) texture :dest-rect dest-rect))
      (sdl2:destroy-texture texture))))

(defmethod render ((line-edge line-edge) buffer)
  (let ((y0 (min (line-edge-y0 line-edge)
                 (line-edge-y1 line-edge)))
        (y1 (max (line-edge-y0 line-edge)
                 (line-edge-y1 line-edge)))
        (start (tree-view-buffer-scroll-y buffer))
        (end (tree-view-display-end buffer)))
    (unless (or (< y1 start) (< end y0))
      (lem-sdl2::set-render-color lem-sdl2::*display* (line-edge-color line-edge))
      (sdl2:render-draw-line (lem-sdl2:current-renderer)
                             (line-edge-x0 line-edge)
                             (- (line-edge-y0 line-edge)
                                start)
                             (line-edge-x1 line-edge)
                             (- (line-edge-y1 line-edge)
                                start)))))

(defun render-all (buffer)
  (loop :for drawable :in (tree-view-buffer-drawables buffer)
        :do (render drawable buffer)))

(defun get-node-at-coordinates (buffer x y)
  (let ((scroll-y (tree-view-buffer-scroll-y buffer)))
    (dolist (drawable (tree-view-buffer-drawables buffer))
      (when (typep drawable 'text-node)
        (let ((node-y (- (text-node-y drawable) scroll-y)))
          (when (and (typep drawable 'text-node)
                     (<= (text-node-x drawable)
                         x
                         (+ (text-node-x drawable)
                            (text-node-width drawable)))
                     (<= node-y
                         y
                         (+ node-y
                            (text-node-height drawable))))
            (return drawable)))))))

(define-major-mode tree-view-mode ()
    (:name "Tree View"))

(defmethod lem-sdl2:render (texture window (buffer tree-view-buffer))
  (sdl2:set-render-target (lem-sdl2:current-renderer) texture)
  (lem-sdl2::set-render-color lem-sdl2::*display*
                              (lem-sdl2::display-background-color lem-sdl2::*display*))
  (sdl2:render-fill-rect (lem-sdl2:current-renderer) nil)
  (render-all buffer))

(defmethod execute ((mode tree-view-mode) (command scroll-down) argument)
  (tree-view-scroll (current-buffer) (* argument 30)))

(defmethod lem-core::handle-mouse-button-down ((buffer tree-view-buffer) mouse-event &key window)
  (multiple-value-bind (x y)
      (lem-core::get-relative-mouse-coordinates-pixels mouse-event window)
    (let ((node (get-node-at-coordinates buffer x y)))
      (when node
        (message "~S ~D ~D" (text-node-value node) x y)))))

(defun make-tree-view-buffer (buffer-name)
  (let ((buffer (make-buffer buffer-name)))
    (change-class buffer 'tree-view-buffer)
    (change-buffer-mode buffer 'tree-view-mode)
    buffer))

(defun draw-tree (buffer-name node)
  (compute-position-with-rightward-extending node)
  (let ((buffer (make-tree-view-buffer buffer-name)))
    (draw buffer node))
  (values))

;;;
(defun make-class-tree (class)
  (let ((subclasses (c2mop:class-direct-subclasses class)))
    (make-instance 'node
                   :value (class-name class)
                   :children (mapcar #'make-class-tree subclasses))))
