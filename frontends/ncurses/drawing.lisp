(defpackage :lem-ncurses/drawing
  (:use :cl
        :lem-core/display))
(in-package :lem-ncurses/drawing)

(defun view-window (view)
  (lem-ncurses/internal::ncurses-view-window view))

(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object void-object))
  0)

(defmethod object-width ((drawing-object text-object))
  (lem-core:string-width (text-object-string drawing-object)))

(defmethod object-width ((drawing-object eol-cursor-object))
  0)

(defmethod object-width ((drawing-object extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object line-end-object))
  0)

(defmethod object-width ((drawing-object image-object))
  0)

(defmethod object-height (drawing-object)
  1)

(defgeneric draw-object (object x y view scrwin))

(defmethod draw-object ((object void-object) x y view scrwin)
  (values))

(defmethod draw-object ((object text-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor (view-window view) x y))
    (lem-ncurses/internal::print-string scrwin x y string attribute)))

(defmethod draw-object ((object eol-cursor-object) x y view scrwin)
  (lem-core::set-last-print-cursor (view-window view) x y)
  (lem-ncurses/internal::print-string
   scrwin
   x
   y
   " "
   (lem:make-attribute :foreground
                       (lem:color-to-hex-string (eol-cursor-object-color object)))))

(defmethod draw-object ((object extend-to-eol-object) x y view scrwin)
  (let ((width (lem-if:view-width (lem-core:implementation) view)))
    (when (< x width)
      (lem-ncurses/internal::print-string
       scrwin
       x
       y
       (make-string (- width x) :initial-element #\space)
       (lem:make-attribute :background
                           (lem:color-to-hex-string (extend-to-eol-object-color object)))))))

(defmethod draw-object ((object line-end-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (lem-ncurses/internal::print-string
     scrwin
     (+ x (line-end-object-offset object))
     y
     string
     attribute)))

(defmethod draw-object ((object image-object) x y view scrwin)
  (values))

(defun render-line-from-behind (view y objects scrwin)
  (loop :with current-x := (lem-if:view-width (lem-core:implementation) view)
        :for object :in objects
        :do (decf current-x (object-width object))
            (draw-object object current-x y view scrwin)))

(defun clear-line (scrwin x y)
  (charms/ll:wmove scrwin y x)
  (charms/ll:wclrtoeol scrwin))

(defun %render-line (view x y objects scrwin)
  (loop :for object :in objects
        :do (draw-object object x y view scrwin)
            (incf x (object-width object))))

(defun render-line (view x y objects)
  (clear-line (lem-ncurses/internal::ncurses-view-scrwin view) x y)
  (%render-line view x y objects (lem-ncurses/internal::ncurses-view-scrwin view)))

(defun render-line-on-modeline (view
                                left-objects
                                right-objects
                                default-attribute)
  (lem-ncurses/internal::print-string (lem-ncurses/internal::ncurses-view-modeline-scrwin view)
                                      0
                                      0
                                      (make-string (lem-ncurses/internal::ncurses-view-width view)
                                                   :initial-element #\space)
                                      default-attribute)
  (%render-line view 0 0 left-objects (lem-ncurses/internal::ncurses-view-modeline-scrwin view))
  (render-line-from-behind view
                           0
                           right-objects
                           (lem-ncurses/internal::ncurses-view-modeline-scrwin view)))

(defun clear-to-end-of-window (view y)
  (let ((win (lem-ncurses/internal::ncurses-view-scrwin view)))
    (when (< y (lem-ncurses/internal::ncurses-view-height view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))

(defun get-char-width ()
  1)
