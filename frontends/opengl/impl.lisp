(in-package :lem-sucle) 

(defclass sucle (lem:implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
    :redraw-after-modifying-floating-window t))
(setf lem:*implementation* (make-instance 'sucle))

(define-condition exit-editor (lem:editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(struct-to-clos:struct->class
 (defstruct ncurses-view
   scrwin
   modeline-scrwin
   x
   y
   width
   height
   lock))

(defmethod lem-if:invoke ((implementation sucle) function)
  (let ((result nil)
        (input-thread (bt:current-thread)))
    (lem.term:term-init)
    (let ((editor-thread
	   (funcall function
		    nil
		    (lambda (report)
		      (bt:interrupt-thread
		       input-thread
		       (lambda ()
			 (print report)
			 (error 'exit-editor :value report)))))))
      (setf result (input-loop editor-thread)))
    (when (and (typep result 'exit-editor)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))

(defmethod lem-if:display-background-mode ((implementation sucle))
  (lem.term:background-mode))

(defmethod lem-if:update-foreground ((implementation sucle) color-name)
  (lem.term:term-set-foreground color-name))

(defmethod lem-if:update-background ((implementation sucle) color-name)
  (lem.term:term-set-background color-name))

(defmethod lem-if:display-width ((implementation sucle))
  (max 5
       ncurses-clone::*columns*
       ;;charms/ll:*cols*
       ))

(defmethod lem-if:display-height ((implementation sucle))
  (max 3
       ncurses-clone::*lines*
       ;;charms/ll:*lines*
       ))

(defmethod lem-if:make-view
    ((implementation sucle) window x y width height use-modeline)
  (flet ((newwin (nlines ncols begin-y begin-x main-screen)
           (declare (ignore main-screen))
           (let ((win
		  (;;charms/ll:newwin
		   ncurses-clone::ncurses-newwin
		   nlines ncols begin-y begin-x)))
	     (when use-modeline (;;charms/ll:keypad
				 ncurses-clone::ncurses-keypad
				 win 1))
             ;; (when main-screen
             ;;   (charms/ll:idlok win 1)
             ;;   (charms/ll:scrollok win 1))
             win)))
    (make-ncurses-view
     :scrwin (newwin height width y x nil)
     :modeline-scrwin (when use-modeline (newwin 1 width (+ y height) x nil))
     :x x
     :y y
     :width width
     :height height
     :lock (bt:make-recursive-lock "window-lock"))))

(defmacro with-view-lock (view &body body)
  (utility::with-gensyms (lock)
    `(let ((,lock (ncurses-view-lock ,view)))
       (bt:with-recursive-lock-held (,lock)
	 ,@body))))


(defmethod lem-if:delete-view ((implementation sucle) view)
  (with-view-lock view
    (;;charms/ll:delwin
     ncurses-clone::ncurses-delwin
     (ncurses-view-scrwin view))
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:delwin
       ncurses-clone::ncurses-delwin
       (ncurses-view-modeline-scrwin view)))))

(defmethod lem-if:clear ((implementation sucle) view)
  ;;;https://linux.die.net/man/3/clearok
  (with-view-lock view
    (;;charms/ll:clearok
     ncurses-clone::ncurses-clearok
     (ncurses-view-scrwin view) 1)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:clearok
       ncurses-clone::ncurses-clearok
       (ncurses-view-modeline-scrwin view) 1))))

(defmethod lem-if:set-view-size ((implementation sucle) view width height)
  (with-view-lock view
    (setf (ncurses-view-width view) width)
    (setf (ncurses-view-height view) height)
    (;;charms/ll:wresize
     ncurses-clone::ncurses-wresize
     (ncurses-view-scrwin view) height width)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:mvwin
       ncurses-clone::ncurses-mvwin
       (ncurses-view-modeline-scrwin view)
       (+ (ncurses-view-y view) height)
       (ncurses-view-x view))
      (;;charms/ll:wresize
       ncurses-clone::ncurses-wresize
       (ncurses-view-modeline-scrwin view)
       (lem:minibuffer-window-height)
       width))))

(defmethod lem-if:set-view-pos ((implementation sucle) view x y)
  (with-view-lock view
    (setf (ncurses-view-x view) x)
    (setf (ncurses-view-y view) y)
    (;;charms/ll:mvwin
     ncurses-clone::ncurses-mvwin
     (ncurses-view-scrwin view) y x)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:mvwin
       ncurses-clone::ncurses-mvwin
       (ncurses-view-modeline-scrwin view)
       (+ y (ncurses-view-height view))
       x))))

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (lem:ensure-attribute attribute-or-name nil))
        (cursorp (eq attribute-or-name 'cursor)))
    (if (null attribute)
        0
        (or (lem::attribute-%internal-value attribute)
            (let* ((foreground (lem:attribute-foreground attribute))
                   (background (lem:attribute-background attribute))
                   (bits (logior (lem.term:get-color-pair foreground background)
                                 0
                                 (if (lem::attribute-bold-p attribute)
                                     ;;charms/ll:a_bold
				     ncurses-clone::a_bold
                                     0)
                                 (if (lem::attribute-underline-p attribute)
                                     ;;charms/ll:a_underline
				     ncurses-clone::a_underline
                                     0)
				 (if (or cursorp (lem::attribute-reverse-p attribute))
				     ncurses-clone::a_reverse
				     0))))
              (setf (lem::attribute-%internal-value attribute) bits)
              bits)))))

(defmethod lem-if:print ((implementation sucle) view x y string attribute)
  (with-view-lock view
    (let ((attr (attribute-to-bits attribute)))
      (;;charms/ll:wattron
       ncurses-clone::ncurses-wattron
       (ncurses-view-scrwin view) attr)
      ;;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
      (;;charms/ll:mvwaddstr
       ncurses-clone::ncurses-mvwaddstr
       (ncurses-view-scrwin view) y x string)
      ;;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
      (;;charms/ll:wattroff
       ncurses-clone::ncurses-wattroff
       (ncurses-view-scrwin view) attr))))

(defmethod lem-if:print-modeline ((implementation sucle) view x y string attribute)
  (with-view-lock view
    (let ((attr (attribute-to-bits attribute)))
      (;;charms/ll:wattron
       ncurses-clone::ncurses-wattron
       (ncurses-view-modeline-scrwin view) attr)
      (;;charms/ll:mvwaddstr
       ncurses-clone::ncurses-mvwaddstr
       (ncurses-view-modeline-scrwin view) y x string)
      (;;charms/ll:wattroff
       ncurses-clone::ncurses-wattroff
       (ncurses-view-modeline-scrwin view) attr))))

(defmethod lem-if:clear-eol ((implementation sucle) view x y)
  (with-view-lock view
    (;;charms/ll:wmove
     ncurses-clone::ncurses-wmove
     (ncurses-view-scrwin view) y x)
    (;;charms/ll:wclrtoeol
     ncurses-clone::ncurses-wclrtoeol
     (ncurses-view-scrwin view))))

(defmethod lem-if:clear-eob ((implementation sucle) view x y)
  (with-view-lock view
    (;;charms/ll:wmove
     ncurses-clone::ncurses-wmove
     (ncurses-view-scrwin view) y x)
    (;;charms/ll:wclrtobot
     ncurses-clone::ncurses-wclrtobot
     (ncurses-view-scrwin view))))

;;(defparameter *no* *standard-output*)

(defmethod lem-if:redraw-view-after ((implementation sucle) view focus-window-p)
  (with-view-lock view
    #+nil ;;;FIXME
    (let ((attr (attribute-to-bits 'modeline)))
      (;;charms/ll:attron
       ncurses-clone::ncurses-attron
       attr)
      #+nil ;;FIXME:: disabling 
      (when (and (ncurses-view-modeline-scrwin view)
		 (< 0 (ncurses-view-x view)))
	(;;charms/ll:move
	 ncurses-clone::ncurses-move
	 (ncurses-view-y view)
	 (1- (ncurses-view-x view)))
	(;;charms/ll:vline
	 ncurses-clone::ncurses-vline
	 (char-code #\space)
	 (1+ (ncurses-view-height view))))
      (;;charms/ll:attroff
       ncurses-clone::ncurses-attroff
       attr)
      (;;charms/ll:wnoutrefresh
       ncurses-clone::ncurses-wnoutrefresh
       ;;charms/ll:*stdscr*
       ncurses-clone::*std-scr*))
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:wnoutrefresh
       ncurses-clone::ncurses-wnoutrefresh
       (ncurses-view-modeline-scrwin view))
      ;;   (ncurses-clone::print-virtual-window ncurses-clone::*virtual-window* *no*)
      )
    
    (;;charms/ll:wnoutrefresh
     ncurses-clone::ncurses-wnoutrefresh
     (ncurses-view-scrwin view)))

  ;;  (ncurses-clone::print-virtual-window ncurses-clone::*virtual-window* *no*)
  )

(defmethod lem-if:update-display ((implementation sucle))
  (let ((view (lem:window-view (lem:current-window))))
    (with-view-lock view
      (let ((scrwin (ncurses-view-scrwin view)))
	(if (lem::covered-with-floating-window-p
	     (lem:current-window)
	     lem::*cursor-x* lem::*cursor-y*)
	    (;;charms/ll:curs-set
	     ncurses-clone::ncurses-curs-set
	     0)
	    (progn
	      (;;charms/ll:curs-set
	       ncurses-clone::ncurses-curs-set
	       1)
	      (;;charms/ll:wmove
	       ncurses-clone::ncurses-wmove
	       scrwin lem::*cursor-y* lem::*cursor-x*)))
	;;FIXME
	(;;charms/ll:wnoutrefresh
	 ncurses-clone::ncurses-wnoutrefresh
	 scrwin)
	(;;charms/ll:doupdate
	 ncurses-clone::ncurses-doupdate)))))

(defmethod lem-if:scroll ((implementation sucle) view n)
  (with-view-lock view
    (;;charms/ll:wscrl
     ncurses-clone::ncurses-wscrl
     (ncurses-view-scrwin view)
     n)))

(defmethod lem-if:clipboard-paste ((implementation sucle))
  (trivial-clipboard:text))

(defmethod lem-if:clipboard-copy ((implementation sucle) text)
  (trivial-clipboard:text text))

(pushnew :lem-sucle *features*)
