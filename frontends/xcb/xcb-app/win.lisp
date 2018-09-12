(in-package :xcb)


(defparameter *w* nil)
;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass win ()
  ((id :accessor id :initform nil)      ;; window xcb id
   (pic-scr :accessor pic-scr :initform nil) ;; xrender extension picture
   (pix-off :accessor pix-off :initform nil) ;; off-screen pixmap id
   (pic-off :accessor pic-off :initform nil) ;; off-screen picture id
   (width   :accessor width   :initform 0)
   (height  :accessor height  :initform 0)

   (%on-resize :accessor %on-resize :initform #'default-%on-resize)
   (%on-key-press :accessor %on-key-press :initform #'default-%on-key-press)
))

(defmethod initialize-instance :after ((win  win) &key w h )
  (with-slots (id pic-scr pic-off width height) win
    (setf width w
	  height h)
    (win-startup win)
   
    win))


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
      ;; off-screen picture
      (multiple-value-setq (pic-off pix-off)
	(new-offscreen-picture width height))

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

(defmethod win-on-configure-notify ((win win) e x y w h)
  (if (or (/= (width win) w)
	  (/= (height win) h))
      (progn (win-on-resize win w h)))
  t)

;;==============================================================================
;; Invoked by win-on-configure-notify, not resize event (which we are not
;; redirecting!)
;;
(defmethod win-on-resize ((win win) w h)
  (with-slots (pic-scr pic-off pix-off width height) win
    (setf width w
	  height h)
    ;; allocate a new off-screen picture and pixmap..
    (check (free-pixmap c pix-off))
    (check (free-picture c pic-off))
    (mvsetq (pic-off pix-off)
	(new-offscreen-picture w h))
    )
  

  t)
;;------------------------------------------------------------------------------
(defmethod win-on-expose ((win win) event)
  (win-refresh win)
  t)
;;------------------------------------------------------------------------------
;; Redraw from off-screen buffer
(defun win-refresh (win)
  (with-slots (pic-scr pic-off width height) win
    (check (composite c OP-SRC pic-off 0 pic-scr 0 0 0 0 0 0 width height))
    (flush c)
))
