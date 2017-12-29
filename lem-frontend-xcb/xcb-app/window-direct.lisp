(in-package :xcb)


(defparameter *w* nil)

(defgeneric destroy (win))
;;-----------------------------------------------------------------------------
;; win protocol
(defgeneric win-on-configure-notify (win event x y w h))
(defgeneric win-on-expose (win event))
(defgeneric win-on-resize (win w h))
(defgeneric win-on-key-press (win key state))
;; simply redraw the window
(defgeneric win-refresh (win))
;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass window-direct ()
  ((id  :accessor id :initform nil)  ;; window xcb id
   (pic :accessor pic :initform nil) ;; draw here picture id
   (width   :accessor width   :initform 0)
   (height  :accessor height  :initform 0)

   (pic-lock :accessor pic-lock :initform nil)
))

(defmethod initialize-instance :after ((win window-direct) &key w h )
  (setf *w* win)
  (with-slots (pic-lock width height) win
    (setf width w
	  height h)
    (win-startup win)
    (setf pic-lock (bt:make-lock))
    win))
;;==============================================================================
;; called by initialize-instance
(defun win-startup (win)
  (with-slots (id pic width height) win
    (setf id (generate-id c) ;; window id
	  pic (generate-id c)) ;;picture id
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
				  EVENT-MASK-KEY-PRESS )) 
	(check (create-window c root-depth id
			      root
			      0 0 width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-EVENT-MASK ;;CW-BACK-PIXEL
				 ) vals)))
      ;; windows get registered
      (window-register id win)
      ;; screen picture for window
      (check (create-picture c pic id +RGB24+ 0 (null-pointer)))
      
      (map-window c id)
      ;; ask WM to send us WM_DELETE_WINDOW message on go-away click
      (w-foreign-values (patom :uint32 +WM-DELETE-WINDOW+)
	(check (change-property c PROP-MODE-REPLACE id +WM-PROTOCOLS+
				4 32 1 patom)))
      (flush c))))



(defmethod destroy ((win window-direct))
  (remhash (id win) windows ))

(defparameter *w* nil)

(defun win-set-name (win name)
  (let ((len (length name))
	(str (foreign-string-alloc name)))
    (w-foreign-values (buf :uint32 8)
      (check (change-property c PROP-MODE-REPLACE (id win) ATOM-WM-NAME
			      ATOM-STRING 8 len str)))
    (foreign-free str)))


;;==============================================================================
;; handle resizing
;;
;; changed:
;; x y  w or h = dragging ul
;; x y         = moving
;; w or h      = resizing

(defmethod win-on-configure-notify ((win window-direct) e x y w h)
  (if (or (/= (width win) w)
	  (/= (height win) h))
      (progn (win-on-resize win w h)))
  t)

;;==============================================================================
;; Synthesized by win-on-configure-notify, not resize event (which we are not
;; redirecting!)
;;
(defmethod win-on-resize ((win window-direct) w h)
  (with-slots (width height) win
    (setf width w
	  height h))
  t)
;;------------------------------------------------------------------------------
(defmethod win-on-expose ((win window-direct) event)
  (win-refresh win)
  t)
;;------------------------------------------------------------------------------
(defmethod win-refresh ((win window-direct))
  ;; ostensibly, we finished drawing
  (xcb::flush c)
  )
