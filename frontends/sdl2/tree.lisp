(defpackage :lem-sdl2/tree
  (:use :cl :lem))
(in-package :lem-sdl2/tree)

(defparameter +margin-y+ 50)
(defparameter +margin-x+ 100)

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

(defun compute-position-with-rightward-extending (node current-y depth)
  (setf (node-x node) depth)
  (cond ((null (node-children node))
         (setf (node-y node) current-y)
         (setf (node-last-y node) current-y)
         (node-y node))
        (t
         (loop :for child :in (node-children node)
               :for i := current-y :then (1+ child-y)
               :for child-y := (compute-position-with-rightward-extending child i (1+ depth))
               :sum (node-y child) :into sum-y
               :finally (setf (node-y node)
                              (/ sum-y (length (node-children node)))))
         (setf (node-last-y node)
               (node-last-y (car (last (node-children node))))))))

(defvar *drawables* '())

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
           :reader text-node-height)))

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

(defun draw-node (node font current-x)
  (let ((y (round (* +margin-y+ (node-y node)))))
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
                           :height node-height)
            *drawables*)
      (dolist (child (node-children node))
        (multiple-value-bind (child-x child-y child-width child-height)
            (draw-node child font (+ +margin-x+ (+ current-x node-width)))
          (declare (ignore child-width))
          (push (make-instance 'line-edge
                               :color (lem:make-color 255 255 255)
                               :x0 (+ current-x (sdl2:surface-width surface))
                               :y0 (+ y (round (sdl2:surface-height surface) 2))
                               :x1 child-x
                               :y1 (+ child-y (round child-height 2)))
                *drawables*)))
      (values current-x
              y
              (sdl2:surface-width surface)
              (sdl2:surface-height surface)))))

(defun draw (node font)
  (let ((*drawables* '()))
    (draw-node node font 0)
    *drawables*))

(defmethod render ((text-node text-node) buffer)
  (when (<= (tree-view-buffer-scroll-y buffer)
            (text-node-y text-node)
            (+ (text-node-y text-node)
               (text-node-height text-node))
            (+ (tree-view-buffer-scroll-y buffer)
               (lem-sdl2::display-height lem-sdl2::*display*)))
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
  (when t #+(or)(<= (tree-view-buffer-scroll-y buffer)
                    (min (line-edge-y0 line-edge)
                         (line-edge-y1 line-edge))
                    (max (line-edge-y0 line-edge)
                         (line-edge-y1 line-edge))
                    (+ (tree-view-buffer-scroll-y buffer)
                       (lem-sdl2::display-height lem-sdl2::*display*)))
    (lem-sdl2::set-render-color lem-sdl2::*display* (line-edge-color line-edge))
    (sdl2:render-draw-line (lem-sdl2:current-renderer)
                           (line-edge-x0 line-edge)
                           (- (line-edge-y0 line-edge)
                              (tree-view-buffer-scroll-y buffer))
                           (line-edge-x1 line-edge)
                           (- (line-edge-y1 line-edge)
                              (tree-view-buffer-scroll-y buffer)))))

(defun render-all (buffer)
  (loop :for object :in (tree-view-buffer-drawables buffer)
        :do (render object buffer)))

(define-major-mode tree-view-mode ()
    (:name "Tree View"))

(defclass tree-view-buffer (text-buffer)
  ((drawables :initarg :drawables
              :accessor tree-view-buffer-drawables)
   (scroll-y :initform 0
             :accessor tree-view-buffer-scroll-y)))

(defmethod lem-sdl2:render (texture window (buffer tree-view-buffer))
  (sdl2:set-render-target (lem-sdl2:current-renderer) texture)
  (lem-sdl2::set-render-color lem-sdl2::*display*
                              (lem-sdl2::display-background-color lem-sdl2::*display*))
  (sdl2:render-clear (lem-sdl2:current-renderer))
  (render-all buffer))

(defmethod execute ((mode tree-view-mode) (command scroll-down) argument)
  (incf (tree-view-buffer-scroll-y (current-buffer)) (* argument 10)))

(defun draw-tree (buffer-name node)
  (let ((buffer (make-buffer buffer-name)))
    (change-class buffer 'tree-view-buffer)
    (compute-position-with-rightward-extending node 0 0)
    (let* ((font (sdl2-ttf:open-font 
                  (lem-sdl2/resource:get-resource-pathname
                   "resources/fonts/NotoSansMono-Regular.ttf")
                  20))
           (drawables (draw node font)))
      (setf (tree-view-buffer-drawables buffer) drawables)
      (change-buffer-mode buffer 'tree-view-mode))))

(defun make-class-tree (class)
  (let ((subclasses (c2mop:class-direct-subclasses class)))
    (make-instance 'node
                   :value (class-name class)
                   :children (mapcar #'make-class-tree subclasses))))
