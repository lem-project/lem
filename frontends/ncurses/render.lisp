(defpackage :lem-ncurses/render
  (:use :cl
        :lem-core/display)
  (:export :render-row
           :render-modeline-row
           :clear-to-end-of-window))
(in-package :lem-ncurses/render)

(defun print-string (scrwin x y string attribute)
  (let ((attr (lem-ncurses/attribute:attribute-to-bits attribute)))
    (charms/ll:wattron scrwin attr)
    (charms/ll:mvwaddstr scrwin y x string)
    (charms/ll:wattroff scrwin attr)))

(defgeneric draw-object (object x y view scrwin))

(defmethod draw-object ((object void-object) x y view scrwin)
  (values))

(defmethod draw-object ((object text-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (when (and attribute (lem:cursor-attribute-p attribute))
      (lem-ncurses/view:set-last-print-cursor view x y)
      (lem:set-attribute-foreground
       attribute
       (lem:color-to-hex-string (lem-if:get-background-color (lem:implementation)))))
    (print-string scrwin x y string attribute)))

(defmethod draw-object ((object eol-cursor-object) x y view scrwin)
  (when (eol-cursor-object-true-cursor-p object)
    (lem-ncurses/view:set-last-print-cursor view x y))
  (print-string
   scrwin
   x
   y
   " "
   (lem:make-attribute :foreground (lem:color-to-hex-string (eol-cursor-object-color object)))))

(defmethod draw-object ((object line-end-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (print-string
     scrwin
     (+ x (line-end-object-offset object))
     y
     string
     attribute)))

(defmethod draw-object ((object image-object) x y view scrwin)
  (values))

(defun clear-line (view x y)
  (charms/ll:wmove (lem-ncurses/view:view-scrwin view) y x)
  (charms/ll:wclrtoeol (lem-ncurses/view:view-scrwin view)))

(defun draw-row (view row scrwin)
  "Draw ROW's background fill, then everything placed on it.
The fill is spaces carrying the background color, since a terminal cell only takes a color by
having a character written into it."
  (let ((width (lem-if:view-width (lem:implementation) view)))
    (when (and (row-fill-color row)
               (< (row-fill-x row) width))
      (print-string scrwin
                    (row-fill-x row)
                    (row-top row)
                    (make-string (- width (row-fill-x row)) :initial-element #\space)
                    (lem:make-attribute :background
                                        (lem:color-to-hex-string (row-fill-color row))))))
  (loop :for placement :in (row-placements row)
        :do (draw-object (placement-object placement)
                         (placement-x placement)
                         (placement-top placement)
                         view
                         scrwin)))

(defun render-row (view row)
  (clear-line view 0 (row-top row))
  (draw-row view row (lem-ncurses/view:view-scrwin view)))

(defun render-modeline-row (view row default-attribute)
  ;; the modeline gets its own curses window, so its row (laid out at top 0) needs no translation.
  (print-string (lem-ncurses/view:view-modeline-scrwin view)
                0
                (row-top row)
                (make-string (lem-ncurses/view:view-width view)
                             :initial-element #\space)
                default-attribute)
  (draw-row view row (lem-ncurses/view:view-modeline-scrwin view)))

(defun clear-to-end-of-window (view y)
  (let ((win (lem-ncurses/view:view-scrwin view)))
    (when (< y (lem-ncurses/view:view-height view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))
