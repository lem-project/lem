(in-package :xcb)

(defclass geometry ()
  ((cell-height :accessor cell-height :initform 14) ;;todo
   (cell-width  :accessor cell-width  :initform 7)
   (cell-baseline :accessor cell-baseline :initform 11)))


(defun cell-rect (geo col row &optional (columns 1) (rows 1))
  "return x y w h for a cell or a group of cells"
  (with-slots (cell-height cell-width) geo
    (values (* cell-width col)
	    (* cell-height row)
	    (* cell-width columns)
	    (* cell-height rows))))


(defun cell-position (geo col row)
    "return x y for a cell"
  (with-slots (cell-height cell-width) geo
    (values (* cell-width col)
	    (* cell-height row))))

(defun cell-base (geo y)
  (with-slots (cell-height cell-baseline) geo
    (-(+ 1 y cell-height) cell-baseline)))

(defun cell-base-position (geo col row)
    "return baseline-y and x"
  (with-slots (cell-height cell-width cell-baseline) geo
    (values (1+ (- (* cell-height (1+ row)) cell-baseline))
	    (* cell-width col))))

(defun cell-grid-calc (geo)
  (with-slots (cell-height cell-width width height) geo
    (values (floor width cell-width)
	     (floor height cell-height )))
  )

