(in-package :xcb)
;;=============================================================================
;; This is the meat of the XCB implementation.
;;
(setf lem::*implementation* :xcb)
#||
(lem:add-hook lem::*before-init-hook*
	      (lambda ())	      )
||#
;;-----------------------------------------------------------------------------
;; ???
(lem:add-hook lem::*after-init-hook*
	      (lambda ()
		(lem:load-theme "emacs-dark")))
;;-----------------------------------------------------------------------------
;; Upon exit, destroy the X window
(lem:add-hook lem::*exit-editor-hook*
	      (lambda ()
		(check (destroy-window c (id *w*)))))

;;=============================================================================
;; Upon initial expose, this handler installs 3 normal handlers for LEM windows
;;
(defun lem-start-%on-expose (win event)
  (declare (ignore event))
  (with-slots (%on-expose %on-key-press %on-resize) win
    (setf %on-expose    #'lem-%on-expose
	  %on-key-press #'lem-%on-key-press
	  %on-resize    #'lem-%on-resize))

  t)
;;-----------------------------------------------------------------------------
(defun lem-%on-expose (win event)
  (format *q* "EXPO~&")
  (lem::redraw-display t)
    t)
;;-----------------------------------------------------------------------------
(defun lem-%on-resize (win w h)
  (lem:send-event :resize)
  t)
;;-----------------------------------------------------------------------------
(defun lem-%on-key-press (win key state)
  (mvbind (keysym mod) (key-process key state)
    (when keysym
      (lem:send-event (lem:make-key
		       :sym     keysym
		       :shift   (logbitp 0 mod)
		       :ctrl (logbitp 1 mod)
		       :meta    (logbitp 2 mod) ;;alt
		       :super   (logbitp 3 mod) ;;win
		       ))))
  t)
;;=============================================================================
;; The main window
;;
(defclass textwin (win geometry)
  ((lem-state :accessor lem-state :initform nil)
   (bg :accessor bg :initform (pen-from-lemcolor "black"));; TODO: theme
   (fg :accessor fg :initform (pen-from-lemcolor "white")))
)
;;-----------------------------------------------------------------------------
;;
(defmethod initialize-instance :after ((win textwin) &key)
  ;; install initial expose handler
  (setf (%on-expose win) #'lem-start-%on-expose)
  (w-foreign-values (hints
		       :UINT32 size-hint-p-resize-inc
		       :UINT32 0 :UINT32 0 ;x y 
		       :UINT32 0 :UINT32 0 ;w h
		       :UINT32 0 :UINT32 0 ;min
		       :UINT32 0 :UINT32 0 ;max
		       :UINT32 (cell-width win)
		       :UINT32 (cell-height win)   ;inc
		       :UINT32 0 :UINT32 0 ;min aspec
		       :UINT32 0 :UINT32 0 ;max aspect
		       :UINT32 0 :UINT32 0 ;base
		       :UINT32 0 ;grav
		       ) 
    (check (xcb::set-wm-normal-hints c (id win) hints)))
  (win-set-name win "XLEM");; TODO: window name
  )


;;==========================================================================
;; window generics are now vectored to handlers
;;
(defmethod win-on-expose ((win textwin) event)
  ;;  (format *q* "ON-EX; editor thread ~A~&" *editor-thread*)
  (funcall (%on-expose win) win event))

(defmethod win-on-resize :after ((win textwin) w h)
;;    (format *q* "ON-RESIZE;  ~A ~A~&" w h)
  (funcall (%on-resize win) win w h))

(defmethod win-on-key-press ((win textwin) key state)
  (funcall (%on-key-press win) win key state))


#||
(defmethod destroy :before ((win textwin))
  (format *q* "destyr~&"))
||#
;;=============================================================================
;; A view is an area of a window:
;;
(defclass textview ()
  ((win :accessor win :initform *w* :initarg :win)
   ;; bounds, in cells
   (vx :accessor vx :initarg :vx)
   (vy :accessor vy :initarg :vy)
   (vw :accessor vw :initarg :vw)
   (vh :accessor vh :initarg :vh)
   (modeline :accessor modeline :initform nil :initarg :modeline)
   (data :accessor data :initform nil :initarg :data)
   ;; context: cursor in cells
   (cx :accessor cx :initform 0)
   (cy :accessor cy :initform 0)))

;;------------------------------------------------------------------------------
;; modeline, if any, is stored as a row index 
(defmethod initialize-instance :after ((tv textview) &key)
  (with-slots (modeline vy vh) tv
    (when modeline
      (setf modeline (+ vy vh)))))



;;------------------------------------------------------------------------------
;; view geometry
(defun view-cell-position (view col row)
  (with-slots (cell-height cell-width) *w*
    (with-slots (vx vy) view
      (values (* cell-width (+ col vx))
	      (* cell-height (+ row vy))))))

(defun view-cell-rect (view col row &optional (columns 1) (rows 1))
  (with-slots (cell-height cell-width) *w*
    (with-slots (vx vy ww hh) view
      (values (* cell-width (+ col vx))
	      (* cell-height (+ row vy))
	      (* cell-width columns)
	      (* cell-height rows)))))

;;==============================================================================
;; debugging
(defparameter *editor-thread* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *xbug* t))

(defmacro xbug (&rest rest)
  (when *xbug*
    `(progn
       (format *q* "xcb: ")
       (format *q* ,@rest))))
;;==============================================================================
(defmethod lem::interface-invoke ((implementation (eql :xcb)) function)
  (xbug "interface-invoke ~&")
  (let ((result nil))
    (unwind-protect
         (progn
	   ;; create x window here, and start the loop.
	   (in) (in1)(make-instance 'textwin :w (* 80 7) :h (* 24 14));TODO
	   (setf *editor-thread* (funcall function))
	   (setf result (input-loop *editor-thread*))) )
    (when (and (typep result 'lem::exit-editor)
               (lem::exit-editor-value result))
      (format *q* "~&exit value: ~A~%" (lem::exit-editor-value result)))))

;; running in input-thread: process x events.  These do not call lem:send-event;
;; key processing and resizing just does its thing.
(defun input-loop (editor-thread)
  (handler-case
      (loop
	 (handler-case
	     (progn
	       (unless (bt:thread-alive-p editor-thread) (return))
	       (force-output *q*)
	       (event-step t)     )
	   #+sbcl
	   (sb-sys:interactive-interrupt  (c)
	     (declare (ignore c))
	     (format *q* "ABORTING~&")
	     (lem:send-abort-event editor-thread t )
	     
		     )))
    (lem::exit-editor (c) (return-from input-loop c)
		      )))

;;==============================================================================
;; Set default colors from LEM
(defmethod lem::interface-update-foreground
    ((implementation (eql :xcb)) color-name)
  (xbug "update-foreground ~A~&" color-name)
  (setf (fg *w*) (pen-create color-name)))
;;==============================================================================
(defmethod lem::interface-update-background
    ((implementation (eql :xcb)) color-name)
  (xbug "update-background ~A~&" color-name)
  (setf (bg *w*) (pen-create color-name)))

;;==============================================================================
;; LEM needs to know if we are light or dark.  This happens when LEM resolves
;; attributes, for instance lem::ensure-attribute.
;;
(defmethod lem::interface-display-background-mode ((implementation (eql :xcb)))
  (xbug "background-mode~&")
  :dark)
;;==============================================================================
;; Display-width request from LEM
;;
(defmethod lem::interface-display-width ((implementation (eql :xcb)))
  (xbug "display-width ~&")
  (nth-value 0 (cell-grid-calc *w*) ))
(defmethod lem::interface-display-height ((implementation (eql :xcb)))
  (xbug "display-height ~&")
  (nth-value 1 (cell-grid-calc *w*) ))



(defparameter *view* nil)
;;==============================================================================
;; Lem wants us to make a view object, which it will cache in the private
;; 
(defmethod lem::interface-make-view
    ((implementation (eql :xcb)) window x y width height use-modeline)
  (xbug "make-view w:~A (~A ~A) ~A by ~A modeline: ~A~&"
	  window x y width height use-modeline)
  (setf *view* (make-instance 'textview
			      :win *w*
			      :vx x :vy y :vw width :vh height
			      :modeline use-modeline
			      :data window)))
;;==============================================================================
;; Delete view  - we dont' have anything 
(defmethod lem::interface-delete-view ((implementation (eql :xcb)) view)
  (xbug "delete-view ~A ~&" view)
;;  (declare (ignore view))
  )
;;==============================================================================
;; clear the entire view
;;
(defmethod lem::interface-clear ((implementation (eql :xcb)) view)
  (xbug "clear-view ~A ~&" view)
  
  (with-slots (vx vy vw vh) view 
    (mvbind (x y w h) (cell-rect *w* vx vy vw vh)
      (pic-rect (pic-scr *w*) (pen-abgr64 (bg *w*)) x y w h))))
;;==============================================================================
(defmethod lem::interface-set-view-size
    ((implementation (eql :xcb)) view width height)
  (xbug "set-view-size ~A ~A ~A ~&" view width height)
  (with-slots (vw vh modeline) view
    (setf vw width
	  vh height)
    (when modeline
      (setf modeline height))))
;;==============================================================================
(defmethod lem::interface-set-view-pos
    ((implementation (eql :xcb)) view x y)
  (xbug "set-view-pos ~A ~A ~A ~&" view x y)
  (with-slots (vx vy) view
    (setf vx x
	  vy y)))

;;==============================================================================
(defmethod lem::interface-print
    ((implementation (eql :xcb)) view x y string attribute)
  (xbug "interface-print ~A (~A ~A) |~A| attr:~A ~&"view x y string attribute)
   (textwin-print view x y string attribute))

(defun textwin-print (view col row string attribute)
  (let* ((slen (length string))
	 (xbuflen (+ (ash slen 2) 8))) ;; 32-bits per character, and header
    (mvbind (xx yy ww hh) (view-cell-rect view col row slen 1)
      ;; get fore and back colors from attribute

      (let (penbg penfg
		  (font *gs-normal*)
		  (a (attribute-check attribute)))
	(when a
	  (destructuring-bind (f . b) (lem::attribute-%internal-value a)
	    (if (lem:attribute-reverse-p a)
		(setf penbg (or f (fg *w*))
		      penfg (or b (bg *w*)))
		(setf penbg (or b (bg *w*))
		      penfg (or f (fg *w*)))))
	  (when (lem:attribute-bold-p a)
	      (setf font *gs-bold*)))
	
	;; any unset colors get defaults
	(unless penbg (setf penbg (bg *w*)))
	(unless penfg (setf penfg (fg *w*)))
	(pic-rect (pic-scr *w*) (pen-abgr64 penbg) xx yy ww hh)
	;; composite text
	(with-foreign-object (xbuf :UINT8 xbuflen)
	  (setf (mem-ref xbuf :UINT32 0) slen ;composite character count
		(mem-ref xbuf :UINT16 4) xx ;window coordinates of x
		(mem-ref xbuf :UINT16 6) (+ yy (cell-baseline *w*) )) ;and y
	  ;; set the glyphs
	  (loop for i from 8 by 4
	     for c across string do
	       (setf (mem-ref xbuf :UINT32 i)
		     (glyph-assure font (char-code c))))
	  
	  (check (composite-glyphs-32
		  c OP-OVER (pen-pic penfg)
		  (pic-scr *w*) +ARGB32+ (glyphset font)
		  0 0 xbuflen xbuf))))
      )))
;;==============================================================================
(defmethod lem::interface-print-modeline
    ((implementation (eql :xcb)) view x y string attribute)
  (xbug "interface-print-modeline: view ~A x: ~A y ~A |~A| ~A~&"
	view x y string attribute)
  (with-slots (modeline) view
    (if modeline
	(textwin-print view x (+ (modeline view) y) string attribute)
	(xbug "No modeline to print~&"))))

;;==============================================================================
(defmethod lem::interface-clear-eol ((implementation (eql :xcb)) view x y)
  (xbug "interface-clear-eol view ~A x: ~A y ~A~&" view x y )
  (with-slots (vx vy vw vh) view 
    (mvbind (xx yy ww hh) (view-cell-rect view x y (- vw x) 1)
      ;; (format *q* "actual rect :~A ~A ~A ~A~&" xx yy ww hh)
      (pic-rect (pic-scr *w*) (pen-abgr64 (bg *w*)) xx yy ww hh)))
  )
;;==============================================================================
(defmethod lem::interface-clear-eob ((implementation (eql :xcb)) view x y)
  (xbug "interface-clear-eol view ~A x: ~A y ~A~&" view x y )
  (with-slots (vx vy vw vh) view
    (if (< y vh)
	(mvbind (xx yy ww hh) (view-cell-rect view x y (- vw x) 1)
	  (pic-rect (pic-scr *w*) (pen-abgr64 (bg *w*)) xx yy ww hh)
	  ;; todo :check 0 case
	;;  (format *q* "Height ~A; y ~A~&" vh y)
	  )
	(format *q* "clear-eob: bad coords"))
    (when (< (incf y) vh)
      ;;(format *q* "Height ~A; y ~A~&" vh y)
      (mvbind (xx yy ww hh) (view-cell-rect view 0 y vw (- vh y))
	(pic-rect (pic-scr *w*) (pen-abgr64 (bg *w*)) xx yy ww hh)))))

;;==============================================================================
;; TODO: when is this useful?
(defmethod lem::interface-move-cursor ((implementation (eql :xcb)) view x y)
  (xbug "move-cursor ~A ~A ~A ~&" view x y)
  (with-slots (cx cy) view
    (setf cx x
	  cy y)))
;;==============================================================================
;; 
(defmethod lem::interface-update-display ((implementation (eql :xcb)))
  (xbug "interface-redraw-display: ~&")
  (xcb::flush c))
;;==============================================================================
(defmethod lem::interface-redraw-view-after
    ((implementation (eql :xcb)) view focus-window-p)
  (xbug "interface-redraw after: view ~A : ~A~&" view focus-window-p)
  (with-slots (modeline vx vy vw vh ) view
    (when (and modeline (< 0 vx))
      (format *q* "HA! ~A ~A ~A ~A~&" vx vy vw vh)
      (mvbind (x y w h) (cell-rect *w* (1- vx) vy 1 (1+ vh))
	(pic-rect (pic-scr *w*) (pen-abgr64 (bg *w*)) x y w h)
	(pic-rect (pic-scr *w*) #xFFFF100020000000 (+ x 3) y 2 h)))))

  

;;==============================================================================
(defmethod lem::interface-scroll ((implementation (eql :xcb)) view n)
  (xbug "interface-scroll ~A ~A~&" view n)
  )
  



;;==============================================================================
;;(ql:quickload :lem-xcb)(in-package :xcb)(in)(in1)
;;(make-instance 'textwin :w 640 :h 480)
;;(lem::interface-make-view :xcb 123 2 3 32 12 nil)
;;(setf *print-base* 16)
;;(dotimes (i 100)(event-step))
;;
;; 21 Dec:
;;(ql:quickload :lem-xcb)(in-package :xcb)
;; (bt:make-thread (lambda () (lem:lem)))
