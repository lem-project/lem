(in-package :xcb)
;;==========================================================================
;;--------------------------------------------------------------------------
;; On a per-connection basis, the windows and events are handled here.
;;
;; Windows are associated to lisp objects via a hashtable, and may be
;; looked up by x ids.
;;
;; Incoming event handlers may use the table to find the lisp object
;;

;;==========================================================================
;; a sensible way to handle event dispatch is jump via a 36-element dispatch
;; table.  Initially filled with defaults.  Pushing a new handler adds it to
;; the handler list for that event.  If the handler returns false, next
;; handler will be called.  The default handler always returns T.
;;
;; The event table is good for the entire connection.
;; resolve window ids to Lisp windows
(defparameter *event-dispatch-table* nil)


(defun event-dispatch-reset ()
  (setf *event-dispatch-table*
	(make-array 36 :initial-element (list #'default-event-handler))))

;;==========================================================================
;; event-type  pull type out of event
;;
(declaim (inline event-type))
(defun event-type (event)
  "return type of event"
  (ldb (byte 7 0) (mem-ref event :uint8))) ;contained in low 7 bits
;;------------------------------------------------------------------------------
;; default  - any unhandled events go here.
;;
(defun default-event-handler (event-pointer)
  (format *q* "~&[Unhandled event: ~A~&" (aref events (event-type event-pointer)))
  t;; last one - always handled.
  )
;;------------------------------------------------------------------------------
;; dispatch an event to a registered or default handler;
;; return event-enum and whatever the handler returns.

(defun dispatch-to-handler-list (handler-list e)
  (let ((result (funcall (car handler-list) e)))
    (unless result (dispatch-to-handler-list (cdr handler-list) e))))


(defun event-dispatch (e)

  (let ((i (event-type e)))
;;    (format *q* "~A~&" (aref events i))
    (if (< i EVENT-LAST-EVENT)
	(dispatch-to-handler-list (aref *event-dispatch-table* i) e)
	(progn
	  (format *q* "UNEXPECTED EVENT ~A~&" i)))
    (foreign-free e)))

(defun event-push-handler (i function)
  (push function (aref *event-dispatch-table* i)))

(defun event-pop-handler (i)
  (if (cdr (aref *event-dispatch-table* i))
      (pop (aref *event-dispatch-table* i))
      (error "Can't pop event handler ~A" (aref events i))))

(defun event-step (&optional (block nil))
  (let ((e (if block
	       (wait-for-event c)
	       (poll-for-event c))))
    (if (null-pointer-p e)
	nil
	(progn (event-dispatch e) t))))
;; process all available events.  Return count of events processed.
(defun events-process ()
  (loop for e = (poll-for-event c)
     for no-more = (null-pointer-p e)
     until no-more do
       (event-dispatch e)
       counting 1))

(defun steps (&optional (num 1))
  (loop for i from 0 below (* num 10) do
     ;;  (format t "...e...") (force-output)
       (event-step)
  ;;     (nv:vin)
       (sleep 0.1)))
(defun event-loop ()
  (loop
     until (zerop (hash-table-count windows))do
       (event-step t)))

;;----------------------------------------------------------------
(defun default-expose (e)
  (with-foreign-slots ((window x y width height count)
			       e (:struct ES-EXPOSE))
    (format *q* "exposing window ~A; (~A ~A) ~A  ~A; ~A more."
	    window x y width height count)
    t))

;; dedicated blocking thread.
(defun event-thread-proc ()
  (loop while t do (event-step t)))
