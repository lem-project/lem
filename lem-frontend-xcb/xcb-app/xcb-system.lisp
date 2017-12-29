(in-package :xcb)
;;=============================================================================
;; Global XCB data

(defparameter c nil) ;; xcb connection
(defparameter s nil) ;; xcb setup
;;(defparameter w nil)
(defparameter root-window nil)
(defparameter +RGB24+ nil)
(defparameter +ARGB32+ nil)

(defparameter +WM-PROTOCOLS+ nil)
(defparameter +WM-DELETE-WINDOW+ nil)

;;==========================================================================
;; window registration
;;
;; global windows hashtable
(defparameter windows (make-hash-table))
;;
;;------------------------------------------------------------------------------
(defun window-register (id object)
  (setf (gethash id windows) object) )
;;------------------------------------------------------------------------------
(defun window-unregister (id)
  (remhash id windows))
;;------------------------------------------------------------------------------
;; retreive window's lisp object
(defun window-object (id)
  (gethash id windows))

(declaim (inline window-object window-register window-unregister))

;;=============================================================================
;; Global initialization
(defun in ()
  (setf c (connect (null-pointer)(null-pointer)))
  (setf s (getf (setup-roots-iterator (get-setup c)) 'data))
  (setf root-window (mem-ref s :uint32))
  (let ((formats (util-query-formats c)))
    (setf +RGB24+ (mem-ref (util-find-standard-format
			  formats PICT-STANDARD-RGB-24) :uint32)
	  +ARGB32+ (mem-ref (util-find-standard-format
			   formats PICT-STANDARD-ARGB-32) :uint32)
	  +WM-PROTOCOLS+ (easy-atom c "WM_PROTOCOLS")
	  +WM-DELETE-WINDOW+ (easy-atom c "WM_DELETE_WINDOW"))))



;;=============================================================================
;; Fonts
(defparameter *font-path-normal* "fonts/DejaVuSansMono.ttf")
(defparameter *font-path-bold* "fonts/DejaVuSansMono-Bold.ttf")

(defparameter *font-normal* nil)
(defparameter *font-bold* nil)

;; session-global initialization...
(defun init-fonts ()
  (ft2init)
  (setf *font-normal*
	(make-instance
	 'font :path
	 (asdf:system-relative-pathname 'lem-xcb *font-path-normal*)
	 :size 10)
	*font-bold*
	(make-instance
	 'font :path
	 (asdf:system-relative-pathname 'lem-xcb *font-path-bold*)
	 :size 10))
  (ft2::get-loaded-advance (face *font-normal*) nil) )

(defun in1 ()
  ;; prepare the event subsystem
  (event-dispatch-reset)
  (event-push-handler EVENT-EXPOSE #'on-expose)
  (event-push-handler EVENT-CLIENT-MESSAGE #'on-client-notify)
  (event-push-handler EVENT-KEY-PRESS #'on-key-press)
  (event-push-handler EVENT-CONFIGURE-NOTIFY #'on-configure-notify)
;;  (event-push-handler EVENT-RESIZE-REQUEST #'on-resize-request)
  (event-push-handler EVENT-DESTROY-NOTIFY #'on-destroy-notify)
;;  (event-push-handler EVENT-MAP-NOTIFY #'on-map-notify)
;  (setf *styles* (make-instance 'styles))
  )
;;------------------------------------------------------------------------------
;; Dispatch expose events via generic win-on-expose
(defun on-expose (event)
  (with-foreign-slots ((window x y width height count) event (:struct ES-EXPOSE))
;;    (format t "ON-EXPOSE; count ~A.  ~A ~A ~A ~A~&" count x y width height)
    (win-on-expose (gethash window windows) event)
    ))
#||
(defun on-resize-request (event)
  (with-foreign-slots ((window width height) event (:struct ES-RESIZE-REQUEST))
    (win-on-resize-request (gethash window windows) event width height)))
||#
;;------------------------------------------------------------------------------
;; Handle window closure
(defun on-client-notify (e)
  (with-foreign-slots ((window type data) e (:struct ES-CLIENT-MESSAGE))
    (when (and (= type +WM-PROTOCOLS+ )
	       (= data +WM-DELETE-WINDOW+))
      (check (destroy-window c window))
      ;;(destroy (gethash window windows))
      t)))

(defun on-destroy-notify (e)
  (with-foreign-slots ((window ) e (:struct ES-DESTROY-NOTIFY))
     (destroy (gethash window windows))

    t)
  )

(defmethod win-on-key-press ((win t) key state))

(defun on-key-press (e)
  (with-foreign-slots ((detail state event) e (:struct ES-INPUT))
;;    (format t "KEYCODE ~X ~A state ~X ~&" detail detail state)
;;    (format t "WINDOW ~A ~&" event)
    (win-on-key-press (gethash event windows) detail state))
  t
  )
(defun on-configure-notify (e)
   (with-foreign-slots (( window x y width height border-width response-type ) e (:struct ES-CONFIGURE))
;;    (format t "CONF ~A ~&" response-type)
    (when (= 150 response-type)
      (win-on-configure-notify (gethash window windows) e x y width height))
    t))
#||
(defun on-resize-notify (e)
  (with-foreign-slots (( window x y width height border-width response-type ) e (:struct ES-CONFIGURE))
    (when (= 150 response-type)
      (win-on-configure-notify (gethash window windows) e x y width height))
    t))
||#

;;=============================================================================
;; create a picture
(defun new-offscreen-picture (width height
				 &optional (value-mask 0)
				   (value-list (null-pointer)))
  "return picture and pixmap ids"
  (with-ids (pixmap picture)
    (check (create-pixmap  c 32 pixmap root-window width height))
    (check (create-picture c picture pixmap +ARGB32+ value-mask value-list))
    (pic-rect picture #xFFFF000000000000 0 0 width height)
    (flush c)
    (values picture pixmap)))

(defun pic-rect (picture color x y width height)
  (w-foreign-values
      (rect :int16 x :int16 y :int16 width :uint16 height)
    (check (fill-rectangles c OP-OVER picture color 1 rect))))


;;=============================================================================
;; event loop






