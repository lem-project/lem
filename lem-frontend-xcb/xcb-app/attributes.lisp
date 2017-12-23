(in-package :xcb)

;;============================================================================
;; A Pen is an xrender picture 1x1 along with a 64-bit #xAAAABBBBGGGGRRRR.
(defstruct pen abgr64 pic)

(defun pen-create (abgr64)
;;  (format *q* "attributes:createing pen ~A~&" abgr64)
  (with-ids (pixmap picture)
    (check (create-pixmap c 32 pixmap root-window 1 1))
    (w-foreign-values (vals :uint32 REPEAT-NORMAL)
      ;; picture
      (check (create-picture c picture pixmap +ARGB32+  CP-REPEAT vals)))
	 ;; fill picture with color (using repeat)
    (w-foreign-values (rect :int16 0 :int16 0 :uint16 1 :uint16 1)
      (check (fill-rectangles c OP-SRC picture abgr64 1 rect)))
    (check (free-pixmap c pixmap));; since it never changes?
    (make-pen :abgr64 abgr64 :pic picture)))
;;------------------------------------------------------------------------------
;; pen-from-lemcolor (which may be "Red" or "#FF0012" or nil.
;; nil pens mean use current default
(defun pen-from-lemcolor (lemcolor)
  (when lemcolor
    (pen-create (rgblist->64 (lemcolor->rgblist lemcolor)))))
;;------------------------------------------------------------------------------
(defun lemcolor->rgblist (lemcolor)
  (check-type lemcolor string)
  (if (char= #\# (char lemcolor 0))
      (list (parse-integer lemcolor :radix 16 :start 5 :end 7)
	    (parse-integer lemcolor :radix 16 :start 3 :end 5)
	    (parse-integer lemcolor :radix 16 :start 1 :end 3) )
      (lem:get-rgb-from-color-name lemcolor)))
;;------------------------------------------------------------------------------
(defun rgblist->64 (rgblist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (logior #xFFFF000000000000
	  (ash (the (unsigned-byte 8) (first rgblist))   8)
	  (ash (the (unsigned-byte 8) (second rgblist)) 24)
	  (ash (the (unsigned-byte 8) (third rgblist))  40)))
;;==============================================================================
;; Attributes
;;
;; Stored in %internal-value: fore and background pens.
;;


;;------------------------------------------------------------------------------
;; attribute-check: Attributes coming from lem may be new; in such a case,
;; create pens and attach attr as %internal
;; 
(defun attribute-check (attribute-or-name)
  (let ((attribute (lem::ensure-attribute attribute-or-name nil)))
    (when attribute
      (unless (lem::attribute-%internal-value attribute)
	(let ((bg-pen (pen-from-lemcolor
		       (lem::attribute-background attribute)))
	      (fg-pen (pen-from-lemcolor
		       (lem::attribute-foreground attribute))))
	  (setf (lem::attribute-%internal-value attribute)
		(cons fg-pen bg-pen))))
#||      (format *q* "ATTRIBUTE-CHECK: ~A ~A ~A ~A ~A~&"
	      (lem::attribute-foreground attribute)
	      (lem::attribute-background attribute)
	      (lem::attribute-reverse-p attribute)
	      (lem::attribute-bold-p attribute)
	      (lem::attribute-underline-p attribute))
||#)

    attribute))

;;------------------------------------------------------------------------------
;; attribute-decode
;;
(defun attribute-decode (attribute)
  "values pen-fg pen-bg boldp underlinep"
  (let ((a (attribute-check attribute))) ; make sure attribute is good
    (if a
	(let ((bold (lem:attribute-bold-p a))
	      (underline (lem:attribute-underline-p a)))
	  (destructuring-bind (f . b) (lem::attribute-%internal-value a)
	    (if (lem:attribute-reverse-p a)
		(values (or b (bg *w*))(or f (fg *w*) bold underline))
		(values (or f (fg *w*))(or b (bg *w*) bold underline)))))
	(values (fg *w*) (bg *w*) nil nil))))

