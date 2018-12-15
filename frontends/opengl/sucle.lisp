(in-package :lem-sucle)

(lem:add-hook
 lem:*before-init-hook*
 (lambda ()
   (lem:load-theme "emacs-dark")))

(defparameter *glyph-height* 16.0)
(defparameter *glyph-width* 8.0)

(defparameter *queue* nil)
(application::deflazy event-queue ()
  (setf *queue* (lparallel.queue:make-queue)))
(application::deflazy virtual-window ((w application::w) (h application::h) (event-queue event-queue))
  (lparallel.queue:push-queue :resize event-queue)
  (setf ncurses-clone::*columns* (floor w *glyph-width*)
	ncurses-clone::*lines* (floor h *glyph-height*))
  (ncurses-clone::with-virtual-window-lock
    (setf ncurses-clone::*virtual-window*
	  (ncurses-clone::make-virtual-window))))

(defparameter *saved-session* nil)
(defun input-loop (editor-thread)
  (setf ncurses-clone::*columns* 80
	ncurses-clone::*lines* 25)
  (setf application::*main-subthread-p* nil)
  (application::main
   (lambda ()
     (block out
       (let ((text-sub::*text-data-what-type* :texture-2d))
	 (handler-case
	     (let ((out-token (list "good" "bye")))
	       (catch out-token
		 (loop
		    (per-frame editor-thread out-token))))
	   (exit-editor (c) (return-from out c))))))
   :width (floor (* ncurses-clone::*columns*
		    *glyph-width*))
   :height (floor (* ncurses-clone::*lines*
		     *glyph-height*))
   :title "lem is an editor for Common Lisp"
   :resizable nil))

(defparameter *last-scroll* 0)
(defparameter *scroll-difference* 0)
(defparameter *scroll-speed* 3)
(defun per-frame (editor-thread out-token)
  (application::on-session-change *saved-session*
    (text-sub::change-color-lookup
     ;;'text-sub::color-fun
     'lem.term::color-fun
     #+nil
     (lambda (n)
       (values-list
	(print (mapcar (lambda (x) (utility::floatify x))
		       (nbutlast (aref lem.term::*colors* n))))))
     )
    (application::refresh 'virtual-window)
    (application::refresh 'event-queue)
    (window::set-vsync t)
    (lem.term::reset-color-pair))
  (application::getfnc 'virtual-window)
  (application::getfnc 'event-queue)
  (application:poll-app)
  (let ((newscroll (floor window::*scroll-y*)))
    (setf *scroll-difference* (- newscroll *last-scroll*))
    (setf *last-scroll* newscroll))

  (glhelp:set-render-area 0 0 window:*width* window:*height*)
  ;;(gl:clear-color 0.0 0.0 0.0 0.0)
  ;;(gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :blend)

  (render-stuff)
  (handler-case
      (progn
	(unless (bt:thread-alive-p editor-thread)
	  (throw out-token nil))
	(resize-event)
	(scroll-event)
	(input-events)	
	(left-click-event)
	#+nil
	(let ((event))
	  
	  (if (eq event :abort)
	      (send-abort-event editor-thread nil)
	      ;;(send-event event)
	      )))
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (lem:send-abort-event editor-thread t))))

(defun left-click-event ()
  (lem:send-event (mouse-event-proc 
		   (window::skey-p
		    (window::mouseval :left)
		    window::*control-state*)
		   (floor window::*mouse-x*
			  *glyph-width*)
		   (floor window::*mouse-y*
			  *glyph-height*))))

;;;mouse stuff copy and pasted from frontends/pdcurses/ncurses-pdcurseswin32
(defvar *dragging-window* ())

(defun mouse-move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point)
                                   x))
(defun mouse-get-window-rect (window)
  (values (lem:window-x      window)
          (lem:window-y      window)
          (lem:window-width  window)
          (lem:window-height window)))

(defun mouse-event-proc (state x1 y1)
  (lambda ()
    (cond
      ;; button1 down
      ((eq state t)
       (let ((press state))
         (find-if
          (lambda(o)
            (multiple-value-bind (x y w h) (mouse-get-window-rect o)
              (cond
                ;; vertical dragging window
                ((and press (= y1 (- y 1)) (<= x x1 (+ x w -1)))
                 (setf *dragging-window* (list o 'y))
                 t)
                ;; horizontal dragging window
                ((and press (= x1 (- x 1)) (<= y y1 (+ y h -2)))
                 (setf *dragging-window* (list o 'x))
                 t)
                ;; move cursor
                ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -2)))
                 (setf (lem:current-window) o)
                 (mouse-move-to-cursor o (- x1 x) (- y1 y))
                 (lem:redraw-display)
                 t)
                (t nil))))
          (lem:window-list))))
      ;; button1 up
      ((null state)
       (let ((o (first *dragging-window*)))
         (when (lem:windowp o)
           (multiple-value-bind (x y w h) (mouse-get-window-rect o)
	     (declare (ignorable x y))
             (setf (lem:current-window) o)
             (cond
               ;; vertical dragging window
               ((eq (second *dragging-window*) 'y)
                (let ((vy (- (- (lem:window-y o) 1) y1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (>= y1       3)
                             (>= (+ h vy) 3))
                    (lem:grow-window vy)
                    (lem:redraw-display))))
               ;; horizontal dragging window
               (t
                (let ((vx (- (- (lem:window-x o) 1) x1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (>= x1       5)
                             (>= (+ w vx) 5))
                    (lem:grow-window-horizontally vx)
                    ;; workaround for display update problem (incomplete)
		    #+nil ;;FIXME
                    (ncurses-clone::ncurses-re
		     ;;force-refresh-display ;;charms/ll:*cols*
		     (- ;;charms/ll:*lines*
		      ncurses-clone::*lines*
		      1
		      ))
                    (lem:redraw-display))))
               )))
         (when o
           (setf *dragging-window*
                 (list nil (list x1 y1) *dragging-window*)))))
      )))


(defun resize-event ()
  (block out
    ;;currently this pops :resize events
    (loop (multiple-value-bind (event exists)
	      (lparallel.queue:try-pop-queue *queue*)
	    (if exists
		(lem:send-event event)
		(return-from out))))))

(defun scroll-event ()
  ;;scrolling
  (let ((scroll *scroll-difference*))
    (unless (zerop scroll)
      (lem:scroll-up (* *scroll-speed* scroll))
      (lem:redraw-display))))
(defun input-events ()
  (let ((array (window::control-state-jp-or-repeat window::*control-state*)))
    (declare (type window::mouse-keyboard-input-array array))
    (dotimes (code 128)
      (let ((true-p (= 1 (sbit array code))))
	(when true-p
	  (multiple-value-bind (name type) (window::back-value code)
	    ;;(print (list name type))
	    (case type
	      (:key
	       (if (window::character-key-p code)
		   (multiple-value-bind (byte)
		       (character-modifier-bits:character-modifier-bits
			(char-code (char-downcase (code-char code)))
			window::*shift*
			window::*control*
			nil;;window::*alt* ;;FIXME -> makes M-X not M-x
			window::*super*)
		     (let ((key (code-to-key byte)))
		       (lem:send-event
			(lem:make-key
			 :sym (lem:key-sym key)
			 :ctrl (or (lem:key-ctrl key)
				   window::*control*)
			 :shift (or (lem:key-shift key)
					;  window::*shift*
				    )
			 :meta (or (lem:key-meta key)
				   window::*alt*)
			 :super (or (lem:key-super key)
				    window::*super*)))))
		   (let ((key (get-sym-from-glfw3-code name)))
		     (if key
			 (lem:send-event (lem:make-key
					  :sym key
					  :meta window::*alt*
					  :super window::*super*
					  :shift window::*shift*
					  :ctrl window::*control*))
			 (format *error-output*
				 "~s key unimplemented" name))))))))))))

(defun render-stuff ()
  #+nil
  (;;text-sub::with-data-shader (uniform rebase)
   ;; (gl:clear :color-buffer-bit)
 ;;   (gl:disable :depth-test)
    #+nil
    (rebase -128.0 -128.0))
  #+nil
  (gl:point-size 1.0)

  ;;;;what? this is to replace (gl:with-primitives :points ...body)
  ;;;; to find bug where resizing the lem window over and over causes crash
  #+nil
  (unwind-protect (progn
		    (gl:begin :points)
		    (opengl-immediate::mesh-vertex-color))
    (gl:end))
  (when ncurses-clone::*update-p*
    (setf ncurses-clone::*update-p* nil)
    ;;;Copy the virtual screen to a c-array,
    ;;;then send the c-array to an opengl texture
    (let* ((c-array-lines
	    (min text-sub::*text-data-height* ;do not send data larger than text data
		 (+ 1 ncurses-clone::*lines*)))              ;width or height
	   (c-array-columns
	    (min text-sub::*text-data-width*
		 (+ 1 ncurses-clone::*columns*)))
	   (c-array-len (* 4
			   c-array-columns
			   c-array-lines)))
      (cffi:with-foreign-object
       (arr :uint8 c-array-len)
       (flet ((color (r g b a x y)
		(let ((base (* 4 (+ x (* y c-array-columns)))))
		  (setf (cffi:mem-ref arr :uint8 (+ 0 base)) r
			(cffi:mem-ref arr :uint8 (+ 1 base)) g
			(cffi:mem-ref arr :uint8 (+ 2 base)) b
			(cffi:mem-ref arr :uint8 (+ 3 base)) a))))
	 (progn
	   (let ((foox (- c-array-columns 1))
		 (bary (- c-array-lines 1)))
	     (flet ((blacken (x y)
		      (color 0 0 0 0 x y)))
	       (blacken foox bary)
	       (dotimes (i bary)
		 (blacken foox i))
	       (dotimes (i foox)
		 (blacken i bary)))))
	 
	 (let ((len ncurses-clone::*lines*))
	   (dotimes (i len)
	     (let ((array (aref ncurses-clone::*virtual-window* (- len i 1))))
	       (dotimes (index ncurses-clone::*columns*)
		 (let* ((glyph (aref array index))
			(attributes (ncurses-clone::glyph-attributes glyph))
			(pair (ncurses-clone::ncurses-color-pair (mod attributes 256))))
		   (flet ((byte/255 (n)
			    (identity n)))
		     (let ((realfg
			    (let ((fg (car pair)))
			      (if (or
				   (not pair)
				   (= -1 fg))
				  (byte/255
				   ncurses-clone::*fg-default*) ;;FIXME :cache?
				  (byte/255
				   fg))))
			   (realbg
			    (let ((bg (cdr pair)))
			      (if (or
				   (not pair)
				   (= -1 bg))
				  (byte/255
				   ncurses-clone::*bg-default*) ;;FIXME :cache?
				  (byte/255
				   bg)))))
		       ;;#+nil
		       (when (logtest ncurses-clone::A_reverse attributes)
			 (rotatef realfg realbg))
		       (color (byte/255
			       (char-code (ncurses-clone::glyph-value glyph)))
			      realfg
			      realbg
			      (byte/255
			       (text-sub::char-attribute
				(logtest ncurses-clone::A_Underline attributes)
				(logtest ncurses-clone::A_bold attributes)
				t))
			      index
			      i))
		     #+nil
		     (vertex (floatify x)
			     (floatify y)
			     0.0))
		   #+nil
		   (incf x)))))))
       ;;;;write the data out to the texture
       (let ((texture (text-sub::get-text-texture)))
	 (gl:bind-texture :texture-2d texture)
	 (gl:tex-sub-image-2d :texture-2d 0 0 0
			      c-array-columns
			      c-array-lines
			      :rgba :unsigned-byte arr)))))
  (text-sub::with-text-shader (uniform)
    (gl:uniform-matrix-4fv
     (uniform :pmv)
     (load-time-value (nsb-cga:identity-matrix))
     nil)   
    (glhelp::bind-default-framebuffer)
    (glhelp:set-render-area 0
			    0
			    (application::getfnc 'application::w)
			    (application::getfnc 'application::h))
    ;#+nil
    (progn
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))

    (glhelp::slow-draw (application::getfnc 'text-sub::fullscreen-quad))))
