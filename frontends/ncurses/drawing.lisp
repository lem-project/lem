(defpackage :lem-ncurses/drawing
  (:use :cl
        :lem-core/display))
(in-package :lem-ncurses/drawing)

(defun view-window (view)
  (lem-ncurses::ncurses-view-window view))

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

(defmethod lem-if:view-width ((implementation lem-ncurses::ncurses) view)
  (lem-ncurses::ncurses-view-width view))

(defmethod lem-if:view-height ((implementation lem-ncurses::ncurses) view)
  (lem-ncurses::ncurses-view-height view))

(defgeneric draw-object (object x y view scrwin))

(defmethod draw-object ((object void-object) x y view scrwin)
  (values))

(defmethod draw-object ((object text-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor (view-window view) x y))
    (lem-ncurses::print-string scrwin x y string attribute)))

(defmethod draw-object ((object eol-cursor-object) x y view scrwin)
  (lem-core::set-last-print-cursor (view-window view) x y)
  (lem-ncurses::print-string
   scrwin
   x
   y
   " "
   (lem:make-attribute :foreground
                       (lem:color-to-hex-string (eol-cursor-object-color object)))))

(defmethod draw-object ((object extend-to-eol-object) x y view scrwin)
  (let ((width (lem-if:view-width (lem-core:implementation) view)))
    (when (< x width)
      (lem-ncurses::print-string
       scrwin
       x
       y
       (make-string (- width x) :initial-element #\space)
       (lem:make-attribute :background
                           (lem:color-to-hex-string (extend-to-eol-object-color object)))))))

(defmethod draw-object ((object line-end-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (lem-ncurses::print-string
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

(defun render-line (view x y objects scrwin)
  (loop :for object :in objects
        :do (draw-object object x y view scrwin)
            (incf x (object-width object))))

(defmethod lem-if:render-line ((implementation lem-ncurses::ncurses) view x y objects height)
  (clear-line (lem-ncurses::ncurses-view-scrwin view) x y)
  (render-line view x y objects (lem-ncurses::ncurses-view-scrwin view)))

(defmethod lem-if:render-line-on-modeline ((implementation lem-ncurses::ncurses)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  (lem-ncurses::print-string (lem-ncurses::ncurses-view-modeline-scrwin view)
                             0
                             0
                             (make-string (lem-ncurses::ncurses-view-width view)
                                          :initial-element #\space)
                             default-attribute)
  (render-line view 0 0 left-objects (lem-ncurses::ncurses-view-modeline-scrwin view))
  (render-line-from-behind view 0 right-objects (lem-ncurses::ncurses-view-modeline-scrwin view)))

(defmethod lem-if:object-width ((implementation lem-ncurses::ncurses) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((implementation lem-ncurses::ncurses) drawing-object)
  1)

(defmethod lem-if:clear-to-end-of-window ((implementation lem-ncurses::ncurses) view y)
  (let ((win (lem-ncurses::ncurses-view-scrwin view)))
    (when (< y (lem-if:view-height implementation view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))

(defmethod lem-if:get-char-width ((implementation lem-ncurses::ncurses))
  1)
