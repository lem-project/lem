(in-package :xcb)
;;=============================================================================
;; This is the meat of the XCB implementation.
;;
(defclass xcb-frontend (lem:implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(setf lem::*implementation* (make-instance 'xcb-frontend))
#||
(lem:add-hook lem::*before-init-hook*
	      (lambda ())	      )
||#
;;-----------------------------------------------------------------------------
;; ???
(lem:add-hook lem::*before-init-hook*
	      (lambda ()
		(lem:load-theme "emacs-dark")))
;;-----------------------------------------------------------------------------
;; Upon exit, destroy the X window
(lem:add-hook lem::*exit-editor-hook*
	      (lambda ()
		;;(format *q* "EXIT-EDITOR HOOK~&")
		(check (destroy-window c (id *w*)))
		(xcb::flush c)))

;;=============================================================================
;; 

;;=============================================================================
;; The main window
;;
(defclass textwin (window-db geometry)
  (;;(lem-state :accessor lem-state :initform nil)
   (bg :accessor bg :initform (pen-from-lemcolor "black"));; TODO: theme
   (fg :accessor fg :initform (pen-from-lemcolor "white")))
)
;;-----------------------------------------------------------------------------
;;

(defmethod initialize-instance :after ((win textwin) &key)
  ;; install initial handler
  (w-foreign-values (hints
		       :UINT32 size-hint-p-resize-inc
		       :UINT32 0 :UINT32 0 ;x y 
		       :UINT32 0 :UINT32 0 ;w h
		       :UINT32 0 :UINT32 0 ;min
		       :UINT32 0 :UINT32 0 ;max
		       :UINT32 (cell-width win)
		       :UINT32  (cell-height win) ;inc
		       :UINT32 0 :UINT32 0 ;min aspec
		       :UINT32 0 :UINT32 0 ;max aspect
		       :UINT32 0 :UINT32 0 ;base
		       :UINT32 0 ;grav
		       ) 
    (check (xcb::set-wm-normal-hints c (id win) hints)))
  (win-set-name win "XLEM");; TODO: window name
  (xcb::flush c)
  )
;;-----------------------------------------------------------------------------
(defmethod win-on-resize :after((win textwin) w h)
  (declare (ignore win w h))
  (lem:send-event :resize)
  t)
;;-----------------------------------------------------------------------------
(defmethod win-on-key-press ((win textwin) key state)
  (declare (ignore win))
  (if (< key 128)
      (mvbind (keysym mod) (key-process key state)
	;; (format *q* "[~A]~A ~A~&" keysym mod  state)
	(when keysym
	  (lem:send-event (lem:make-key
			   :sym     keysym
			   :shift   (logbitp 0 mod)
			   :ctrl (logbitp 1 mod)
			   :meta    (logbitp 2 mod) ;;alt
			   :super   (logbitp 3 mod) ;;win
			   ))))
      (format *q* "Invalid key ~X~&" key))
  t)


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

;;==============================================================================
(defmethod lem::interface-invoke ((implementation xcb-frontend) function)
  (xbug "interface-invoke ~A~&" (bt:current-thread))
  (let ((result nil))
    (unwind-protect
	 (in) (in1)
	 (setf *editor-thread*
	       (funcall function
			(lambda ()
			  (let* ((width (truncate (init-fonts)))
				 (w (make-instance 'textwin :w (* 80 width) :h (* 24 14))))
			    (setf (cell-width w) width)))))

     	      
      (setf result (input-loop *editor-thread*)))

    (when (and (typep result 'lem::exit-editor)
               (lem::exit-editor-value result))
;;      (format *q* "~&exit value: ~A~%" (lem::exit-editor-value result))
      )))


;; running in input-thread: process x events.  These do not call lem:send-event;
;; key processing and resizing just does its thing.
(defun input-loop (editor-thread)
  (handler-case
      (loop
	 (handler-case
	     (progn
	       (unless (bt:thread-alive-p editor-thread) (return))
	     ;;  (force-output *q*)
	       (event-step t)     )
	   #+sbcl
	   (sb-sys:interactive-interrupt  (cc)
	     (declare (ignore cc))
	     (format *q* "ABORTING~&")
	     (lem:send-abort-event editor-thread t ))))
    (lem::exit-editor (cc) (return-from input-loop cc)
		      )))

;;==============================================================================
;; Set default colors from LEM
(defmethod lem::interface-update-foreground
    ((implementation xcb-frontend) color-name)
  (xbug "update-foreground ~A~&" color-name)
  (setf (fg *w*) (pen-create color-name)))
;;==============================================================================
(defmethod lem::interface-update-background
    ((implementation xcb-frontend) color-name)
  (xbug "update-background ~A~&" color-name)
  (setf (bg *w*) (pen-create color-name)))

;;==============================================================================
;; LEM needs to know if we are light or dark.  This happens when LEM resolves
;; attributes, for instance lem::ensure-attribute.
;;
(defmethod lem::interface-display-background-mode ((implementation xcb-frontend))
  (xbug "background-mode~&")
  :dark)
;;==============================================================================
;; Display-width request from LEM
;;
(defmethod lem::interface-display-width ((implementation xcb-frontend))
  (xbug "display-width ~&")
  (nth-value 0 (cell-grid-calc *w*) ))
(defmethod lem::interface-display-height ((implementation xcb-frontend))
  (xbug "display-height ~&")
  (nth-value 1 (cell-grid-calc *w*) ))




;;==============================================================================
;; Lem wants us to make a view object, which it will cache in the private
;; 
(defmethod lem::interface-make-view
    ((implementation xcb-frontend) window x y width height use-modeline)
  ;;  (xbug "make-view w:~A (~A ~A) ~A by ~A modeline: ~A~&"	window x y width height use-modeline)
  (make-instance 'textview
		 :win *w*
		 :vx x :vy y :vw width :vh height
		 :modeline (and use-modeline
				height)
		 :data window))
;;==============================================================================
;; Delete view  - we dont' have anything 
(defmethod lem::interface-delete-view ((implementation xcb-frontend) view)
  (xbug "delete-view ~A ~&" view)
  ;; TODO: fix this! https://github.com/cxxxr/lem/issues/101
  ;; For now, force YET ANOTHER REDRAW only to make the modeline refresh! 
 ;; (lem::redraw-display t)
  )
;;==============================================================================
;; clear the entire view  -- is this ever called?
;;
(defmethod lem::interface-clear ((implementation xcb-frontend) view)
  (bt:with-lock-held ((pic-lock *w*))
    (xbug "clear-view ~A ~&" view)
    (with-slots (vx vy vw vh) view 
      (mvbind (x y w h) (cell-rect *w* vx vy vw vh)
	(pic-rect (pic *w*) (pen-abgr64 (bg *w*)) x y w h)))))
;;==============================================================================
(defmethod lem::interface-set-view-size
    ((implementation xcb-frontend) view width height)
  (bt:with-lock-held ((pic-lock *w*))
    (xbug  "set-view-size <~A>~A ~A ~A (old pos ~A ~A) ~&"
	   (bt:current-thread)
	   view width height
	   (vx view) (vy view))
    (with-slots (vw vh modeline) view
      (setf vw width
	    vh height)
      (when modeline
	(setf modeline height)))))
;;==============================================================================
(defmethod lem::interface-set-view-pos
    ((implementation xcb-frontend) view x y)
  (xbug "set-view-pos ~A ~A ~A ~&" view x y)
  (with-slots (vx vy) view
    (setf vx x
	  vy y)))

;;==============================================================================
(defmethod lem::interface-print
    ((implementation xcb-frontend) view x y string attribute)
  (bt:with-lock-held ((pic-lock *w*))
    (xbug "interface-print <~A>~A (~A ~A) |~A| attr:~A ~&" (bt:current-thread) view x y string attribute)
    (textwin-print view x y string attribute)))

;; return attribute values

(defun textwin-print (view col row string attribute)
  ;;(format *q* "1printing at (~A ~A) of view at (~A ~A)~&"col row (vx view) (vy view))
  (let* ((slen (length string))
	 (xbuflen (+ (ash slen 2) 8))) ;; 32-bits per character, and header
    (mvbind (xx yy ww hh) (view-cell-rect view col row slen 1)
      (mvbind (fg bg boldp underlinep) (attribute-decode attribute)
		(let ((font (if boldp *font-bold* *font-normal*)))
	  (pic-rect (pic *w*) (pen-abgr64 bg) xx yy ww hh)
	  (when underlinep
	    (pic-rect (pic *w*) (pen-abgr64 fg)
		      xx (+ yy -1 (cell-height *w*))
		      ww 1))
;;	  (format *q* "printing ~A chars at (~A ~A) ~A ~A ~&" slen xx yy ww hh)
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
		    c OP-OVER (pen-pic fg)
		    (pic *w*) +ARGB32+ (glyphset font)
		    0 0 xbuflen xbuf))))))))
;;==============================================================================
(defmethod lem::interface-print-modeline
    ((implementation xcb-frontend) view x y string attribute)
  (with-slots (modeline) view
    (bt:with-lock-held ((pic-lock *w*))
      (xbug "interface-print-modeline: view ~A x: ~A y ~A |~A| ~A~&"
	    view x y string attribute)
      (if modeline
	  (textwin-print view x (+ modeline y) string attribute)
	  (xbug "No modeline to print~&")))))

;;==============================================================================
(defmethod lem::interface-clear-eol ((implementation xcb-frontend) view x y)
  (with-slots (vx vy vw vh) view
    (bt:with-lock-held ((pic-lock *w*))
      (xbug "interface-clear-eol view ~A x: ~A y ~A~&" view x y )
      (mvbind (xx yy ww hh) (view-cell-rect view x y (- vw x) 1)
	(pic-rect (pic *w*) (pen-abgr64 (bg *w*)) xx yy ww hh)))))
;;==============================================================================
(defmethod lem::interface-clear-eob ((implementation xcb-frontend) view x y)
  (with-slots (vx vy vw vh) view
    (bt:with-lock-held ((pic-lock *w*))
      (xbug "interface-clear-eol view ~A x: ~A y ~A~&" view x y )
      (if (< y vh)
	  (mvbind (xx yy ww hh) (view-cell-rect view x y (- vw x) 1)
	    (pic-rect (pic *w*) (pen-abgr64 (bg *w*)) xx yy ww hh)
	    ;; todo :check 0 case
	    ;;  (format *q* "Height ~A; y ~A~&" vh y)
	    )
	 (format *q* "clear-eob: bad coords"))
      (when (< (incf y) vh)
       ;;(format *q* "Height ~A; y ~A~&" vh y)
       (mvbind (xx yy ww hh) (view-cell-rect view 0 y vw (- vh y))
	 (pic-rect (pic *w*) (pen-abgr64 (bg *w*)) xx yy ww hh))))))

;;==============================================================================
;; TODO: when is this useful?
(defmethod lem::interface-move-cursor ((implementation xcb-frontend) view x y)
  (xbug "move-cursor ~A ~A ~A ~&" view x y)
  (with-slots (cx cy) view
    (setf cx x
	  cy y)))
;;==============================================================================
;; 
(defmethod lem::interface-update-display ((implementation xcb-frontend))
  (xbug "interface-redraw-display: ~&")
					;  (xcb::flush c)
  (win-refresh *w*))
;;==============================================================================
(defmethod lem::interface-redraw-view-after
    ((implementation xcb-frontend) view focus-window-p)
  (bt:with-lock-held ((pic-lock *w*))
    (xbug "interface-redraw after: view ~A : ~A~&" view focus-window-p)
    (with-slots (modeline vx vy vw vh ) view
      (when (> vx 0)
	;; draw a vertical separator if view not leftmost
	(mvbind (x y w h) (cell-rect *w* (1- vx) vy 1 vh)
	  (pic-rect (pic *w*) (pen-abgr64 (bg *w*)) x y w h))
	(when modeline;; draw modeline divet
	  (mvbind (x y w h) (cell-rect *w* (1- vx) modeline 1 1)
	    (pic-rect (pic *w*) #xFFFF800080008000 x y w h))))
      ;; Focused windows get an outline
      (let ((outline-color  (if focus-window-p
				#x7FFF200040000000
				#x7FFF100020000000)))
	(mvbind (x y w h) (cell-rect *w* vx vy vw vh)
	  (pic-rect (pic *w*) outline-color (- x 1)  y w 1) ;; top
	  (pic-rect (pic *w*) outline-color (+ x w -1 ) y 1 h) ;; right
	  (pic-rect (pic *w*) outline-color x (+ y h -1) w 1) ;; bottom
	  (pic-rect (pic *w*) outline-color (- x 1) y 1 h) ;; left
	  )))))

;;==============================================================================
(defmethod lem::interface-scroll ((implementation xcb-frontend) view n)
  (xbug "interface-scroll ~A ~A~&" view n))
  



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
