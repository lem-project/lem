(in-package :xcb)
;;
;; Strategy: Change exposed xcb handle for pic to an offscreen buffer;
;; refresh will copy to screen.
;;
;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass window-db (window-direct)
  ((pix-off :accessor pix-off :initform nil) ;; off-screen pixmap id
   (pic-screen :accessor pic-screen :initform nil) ;;
   
))

;;=============================================================================
;; After initializing the screen, convert pic to be an offscreen buffer.
;;
(defmethod initialize-instance :after ((win  window-db) &key )
  (with-slots (pic pix-off pic-screen width height) win
    (setf pic-screen pic); original screen buffer
     ;; off-screen picture
    (mvsetq (pic pix-off)
	(new-offscreen-picture width height))
    (flush c)
    win))


(defmethod destroy :before ((win window-db))
  (with-slots (pic pix-off id ) win
    (check (free-pixmap c pix-off))
    (check (free-picture c pic))))


(defmethod win-refresh ((win window-db))
  (with-slots (pic pic-screen width height) win
    (check (composite c OP-SRC pic 0 pic-screen 0 0 0 0 0 0 width height))
    (flush c)))
;;==============================================================================
;; Invoked by win-on-configure-notify, not resize event (which we are not
;; redirecting!)
;;
;; Todo: check if this is thread-safe.  Slowing it down, by debug output,
;; causes intermittent crashes.  Probably due to thread going to sleep on
;; output!, so possibly, not an issue.
(defmethod win-on-resize :after ((win window-db) w h)
  (with-slots ( pic pic-lock pix-off width height) win
    ;; allocate a new off-screen picture and pixmap..
    (bt:with-lock-held (pic-lock)
      (check (free-pixmap c pix-off))
      (check (free-picture c pic))
      (mvsetq (pic pix-off)
	  (new-offscreen-picture w h))))
  t)
;;------------------------------------------------------------------------------
(defmethod win-on-expose ((win window-db) event)
  (win-refresh win)
  t)
