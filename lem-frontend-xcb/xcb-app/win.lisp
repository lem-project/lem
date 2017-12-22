(in-package :xcb)


(defparameter *w* nil)
;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass win ()
  ((id :accessor id :initform nil)      ;; window xcb id
   (pic-scr :accessor pic-scr :initform nil) ;; xrender extension picture
   (width   :accessor width   :initform 0)
   (height  :accessor height  :initform 0)

   (%on-expose :accessor %on-expose :initform #'default-%on-expose)
   (%on-resize :accessor %on-resize :initform #'default-%on-resize)
   (%on-key-press :accessor %on-key-press :initform #'default-%on-key-press)
))

(defmethod initialize-instance :after ((win  win) &key w h )
  (with-slots (id pic-scr pic-off width height) win
    (setf width w
	  height h)
    (win-startup win)
   
    win))

(defun default-%on-expose (win event)
  (declare (ignore win event))
 #|| (with-slots (%on-expose %on-key-press %on-resize) win
    (setf %on-expose    #'lem-%on-expose
	  %on-key-press #'lem-%on-key-press
	  %on-resize    #'lem-%on-resize)
    (lem:lem))
||#
  t)

(defun default-%on-resize (win w h)
  (declare (ignore win w h))
  t)

(defun default-%on-key-press (win key state)
  (declare (ignore win key state))
  t)







(defun win-startup (win)
  (SETF *W* WIN)
  (with-slots (id pic-scr pix-off pic-off width height) win
    (setf id (generate-id c)
	  pic-scr (generate-id c))
    (with-foreign-slots ((root root-visual white-pixel black-pixel
			       root-depth) s
			 (:struct screen-t))
      (w-foreign-values (vals
			 ;;:uint32 white-pixel
			 :uint32 (+
				  EVENT-MASK-EXPOSURE
				  EVENT-MASK-STRUCTURE-NOTIFY
				  ;;EVENT-MASK-RESIZE-REDIRECT
				  ;; EVENT-MASK-BUTTON-PRESS
				  EVENT-MASK-KEY-PRESS
				 
				  )
			 ) 
	(check (create-window c root-depth id
			      root
			      0 0 width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-EVENT-MASK ;;CW-BACK-PIXEL
				 ) vals)))
      (window-register id win)
      ;; screen picture
      (check (create-picture c pic-scr id +RGB24+ 0 (null-pointer)))
      
      (map-window c id)
      ;; ask WM to send us WM_DELETE_WINDOW message on go-away click
      (w-foreign-values (patom :uint32 +WM-DELETE-WINDOW+)
	(check (change-property c PROP-MODE-REPLACE id +WM-PROTOCOLS+
				4 32 1 patom)))
      (flush c))))

;; with some assumptions: c root-window

(defmethod destroy ((win win))
  (with-slots (pic-scr pic-off pix-off id ) win
    (remhash id windows )))

(defparameter *w* nil)

(defun win-set-name (win name)
  (let ((len (length name))
	(str (foreign-string-alloc name)))
    (w-foreign-values (buf :uint32 8)
      (check (change-property c PROP-MODE-REPLACE (id win) ATOM-WM-NAME
			      ATOM-STRING 8 len str)))
    (foreign-free str))
  )


;;==============================================================================
;; handle resizing
;;
;; changed:
;; x y  w or h = dragging ul
;; x y         = moving
;; w or h      = resizing
(let((rx 0) (ry 0) (rw 0)(rh 0))
  (defmethod win-on-configure-notify ((win win) e x y w h)
    
;;    (format t "CONF ~A ~A ~A ~A~&" x y w h)
    (if (or (/= (width win) w)
	    (/= (height win) h))
	
	(progn ;;(format *q* "RESIZING ~A ~A&" h rh)
	       (win-on-resize win w h)))
     ;; still resizing? interactively fill the blank spaces.
#||

    (with-slots (pic-scr width height) win
      (when (> w width)
	;;(format t "right: ~A ~A ~A ~A~&"      width 0 (- w width) height)
	(pic-rect pic-scr #xFFFF000000000000  width 0 (- w width) height))
      (when (> h height)
	;;(format t "bot: ~A ~A ~A ~A~&"  0 height w (- h height))
	(pic-rect pic-scr #xFFFF000000000000  0 height w (- h height) )))
  ||#  (setf rx x
	  ry y
	  rw w
	  rh h) 

    t)
)
;;==============================================================================
;; Invoked by win-on-configure-notify, not resize event (which we are not
;; redirecting!)
;;
(defmethod win-on-resize ((win win) w h)
  (with-slots (pic-scr width height) win
    (setf width w
	  height h))
  t)

(defmethod win-on-expose ((win win) event)
  ;; just to make the class concrete

  t)


