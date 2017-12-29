(in-package :xcb)
;;==============================================================================
;;
;; The freetype2 library is used to extract glyph bitmaps and convert them to
;; XRender ABGR format.
;;
;; The client provides glyph information as well.
;;
;; The client must also track fonts
(defparameter gs nil)

;; missing from ft2 due to old age...
(defcfun ("FT_Library_SetLcdFilter" set-lcd-filter&) :uint32
 (library ft2::ft-library)
  (filter :uint32))

(defparameter *xcb-context* nil)
(defun ft2init ()
  (let ((result  (set-lcd-filter& ft2::*library* 1)))
    (setf *xcb-context* c)
      ))

(defclass font ()
  ((face :accessor face :initform nil)
   (glyphset :accessor glyphset :initform nil)
   (map1k :accessor map1k :initform (make-array 1024
						:element-type 'bit
						:initial-element 0))
   (mapbig :accessor mapbig :initform (make-hash-table )))
  )

(defmethod initialize-instance :after ((f font) &key path size)
  (with-slots (face glyphset) f
    (setf face (ft2:new-face path))
    (ft2:set-char-size face (* size 64)(* size 64) 85 88)
    
    (setf glyphset (generate-id c))
    (check (create-glyph-set c glyphset +ARGB32+ ))
    ;; low glyphs are always loaded
    (loop for n from 32 to 127 do
	  (load-glyph f n))
      ;;      (load-glyph f gs 992)an
      ;;      (load-glyph f gs 994)
      ;;      (load-glyph f gs 1046)

    (values gs f)))

(defmethod destroy ((f font))
  (with-slots (glyphset face) f
    (check (free-glyph-set c glyphset))
    (setf face nil)))
;;==============================================================================
;; Glyph cache check: all glyphs < 256 are loaded. <1024 are checked with a
;; bit vector map1k ; rest - mapbig hashtable
;;==============================================================================
(defun glyph-assure-long (font code)
  (declare (type fixnum code))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (face glyphset map1k mapbig) font
    (if (zerop (logand code #xFFFC00))
	(when (zerop (bit map1k code))
	  (load-glyph font code)
	  (setf (bit map1k code) 1))
	(unless (gethash code mapbig)
	  (load-glyph font code)
	  (setf (gethash code mapbig) t)))
    code))

(defun glyph-assure (font code)
;;  (format *q* "GLYPH-ASSURE.  Thread: ~A; c is |~A|~&" (bt:current-thread) *xcb-context*)
  (if (> code 256)
       (glyph-assure-long font code )
       code))

#||
;; load a glyphset
(defun load-gs (filename size )
    
    (let ((f (ft2:new-face filename)))
    (ft2:set-char-size f (* size 64)(* size 64) 85 88)

    (let* ((gs (generate-id c))
	   (cookie (create-glyph-set-checked c gs +ARGB32+ )))
      (unless (null-pointer-p (request-check c cookie))
	(error "at: create-glyph-set"))
      (loop for n from 32 to 127 do	   (load-glyph f gs n))
      (load-glyph f gs 992)
      (load-glyph f gs 994)
      (load-glyph f gs 1046)
      (values gs f))))
||#
;;==============================================================================
;; make an x glyph bitmap, and convert rows.  Free later!
(defun make-glyph-bitmap (source w h s-pitch )
   (let* ((d-pitch (* 4 w))
	 (bitmap (foreign-alloc :uint32 :count (* d-pitch h))))
    (flet ((convert-row (s-off d-off)
	     (loop for i from 0 below w
		for s-off from s-off by 3 
		for d-off from d-off by 4 do
		  (let ((srgb (mem-ref source :uint32 s-off)))
		    (setf (mem-ref bitmap :uint32 d-off)
			  (logior (logand srgb #x00FF00)
				  (ldb (byte 8 16) srgb)
				  (ash (ldb (byte 8 0) srgb) 16)))))))
      (unless (zerop w)
	(loop for row from 0 below h
	   for s-off from 0 by s-pitch
	   for d-off from 0 by d-pitch do
	     (convert-row s-off d-off))))
    bitmap))

;;==============================================================================
;; create a foreign glyphinfo-t
;;
(defun make-glyphinfo (w h x y x-adv y-adv)
  (foreign-values
   :uint16 w
   :uint16 h
   :int16  x
   :int16  y
   :int16 x-adv
   :int16  y-adv))
;;==============================================================================
;; dump resultant bitmap for debug only
(defun dump-bitmap (ptr w h)
  (terpri)
  (loop for y from 0 below h do
       (loop for x from 0 below w do
	    (format t "~8x " (ash (mem-ref ptr :uint32) -8))
	    (cffi::incf-pointer ptr 4))
       (terpri)))
;;==============================================================================
;;  ;; #x30024=  0011 0000 0000 0010 1000 = render,lcd autohint
;; TODO: if this is really how we do it, more than one glyph may be sent to X
(defun load-glyph (font code)
  (with-slots (face glyphset) font
    (let* ((ft2-glyph-index (ft2:get-char-index face code))
	   (unused  (ft2:load-glyph face ft2-glyph-index #x30024 ))
	   (glyphslot (ft2::ft-face-glyph face))
	   (bitmap    (ft2::ft-glyphslot-bitmap glyphslot))
	   (left      (ft2::ft-glyphslot-bitmap-left glyphslot))
	   (top       (ft2::ft-glyphslot-bitmap-top  glyphslot))
	   (advance-x  ;;(ft2::get-loaded-advance face nil)
	    (ft2::ft-vector-x (ft2::ft-glyphslot-advance glyphslot)))
	   ;; and
	   (w         (/ (ft2::ft-bitmap-width bitmap) 3))
	   (h         (ft2::ft-bitmap-rows bitmap))
	   (s-pitch        (ft2::ft-bitmap-pitch bitmap))
	   (source         (ft2::ft-bitmap-buffer bitmap))
	   ;;	 (qqq  (format t "~A: ~A, ~A ~A ~A ~A~&" code advance-x w h s-pitch source))
	   (x-bitmap  (make-glyph-bitmap source w h s-pitch))
	   (glyphinfo (make-glyphinfo w h (- left)  top (/ advance-x 64) 0)))
      (declare (ignore unused))
      ;;(format *q* "WWWWW ~A ~A ~A ~&" (code-char code) (/ advance-x 64) (ft2::get-loaded-advance face nil))
      (w-foreign-values (pcode :uint32 code)
	(check (add-glyphs *xcb-context* glyphset  1 pcode  glyphinfo (* 4 w h) x-bitmap)))
      (foreign-free x-bitmap)
      (foreign-free glyphinfo)
      ;; mark glyph as loaded
      
      )))








