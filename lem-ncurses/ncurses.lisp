(in-package :lem)

(defstruct ncurses-view
  scrwin
  modeline-scrwin
  x
  y
  width
  height)

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (ensure-attribute attribute-or-name nil)))
    (if (null attribute)
        0
        (or (lem::attribute-%internal-value attribute)
            (let ((bits (logior (lem.term:get-color-pair (lem::attribute-foreground attribute)
                                                         (lem::attribute-background attribute))
                                (if (lem::attribute-reverse-p attribute)
                                    charms/ll:a_reverse
                                    0)
                                (if (lem::attribute-bold-p attribute)
                                    charms/ll:a_bold
                                    0)
                                (if (lem::attribute-underline-p attribute)
                                    charms/ll:a_underline
                                    0))))
              (setf (lem::attribute-%internal-value attribute) bits)
              bits)))))

(defmethod interface-invoke ((implementation (eql :ncurses)) function)
  (unwind-protect
       (progn
         (lem.term:term-init)
         (let ((editor-thread (funcall function)))
           (input-loop editor-thread)))
    (lem.term:term-finalize)))

(defmethod interface-display-background-mode ((implementation (eql :ncurses)))
  (lem.term:background-mode))

(defmethod interface-update-foreground ((implementation (eql :ncurses)) color-name)
  (lem.term:term-set-foreground color-name))

(defmethod interface-update-background ((implementation (eql :ncurses)) color-name)
  (lem.term:term-set-background color-name))

(defmethod interface-display-width ((implementation (eql :ncurses)))
  (max 5 charms/ll:*cols*))

(defmethod interface-display-height ((implementation (eql :ncurses)))
  (max 3 charms/ll:*lines*))

(defmethod interface-make-view ((implementation (eql :ncurses)) x y width height use-modeline-p)
  (flet ((newwin (nlines ncols begin-y begin-x)
           (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
             (charms/ll:keypad win 1)
             win)))
    (make-ncurses-view
     :scrwin (newwin height width y x)
     :modeline-scrwin (when use-modeline-p (newwin 1 width (+ y height) x))
     :x x
     :y y
     :width width
     :height height)))

(defmethod interface-delete-view ((implementation (eql :ncurses)) view)
  (charms/ll:delwin (ncurses-view-scrwin view))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:delwin (ncurses-view-modeline-scrwin view))))

(defmethod interface-clear ((implementation (eql :ncurses)) view)
  (charms/ll:clearok (ncurses-view-scrwin view) 1)
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:clearok (ncurses-view-modeline-scrwin view) 1)))

(defmethod interface-set-view-size ((implementation (eql :ncurses)) view width height)
  (setf (ncurses-view-width view) width)
  (setf (ncurses-view-height view) height)
  (charms/ll:wresize (ncurses-view-scrwin view) height width)
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
                     (+ (ncurses-view-y view) height)
                     (ncurses-view-x view))
    (charms/ll:wresize (ncurses-view-modeline-scrwin view)
                       (minibuffer-window-height)
                       width)))

(defmethod interface-set-view-pos ((implementation (eql :ncurses)) view x y)
  (setf (ncurses-view-x view) x)
  (setf (ncurses-view-y view) y)
  (charms/ll:mvwin (ncurses-view-scrwin view) y x)
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
                     (+ y (ncurses-view-height view))
                     x)))

(defmethod interface-print ((implementation (eql :ncurses)) view x y string attribute)
  (let ((attr (attribute-to-bits attribute)))
    (charms/ll:wattron (ncurses-view-scrwin view) attr)
    (charms/ll:mvwaddstr (ncurses-view-scrwin view) y x string)
    (charms/ll:wattroff (ncurses-view-scrwin view) attr)))

(defmethod interface-print-modeline ((implementation (eql :ncurses)) view x y string attribute)
  (let ((attr (attribute-to-bits attribute)))
    (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
    (charms/ll:mvwaddstr (ncurses-view-modeline-scrwin view) y x string)
    (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)))

(defmethod interface-clear-eol ((implementation (eql :ncurses)) view x y)
  (charms/ll:wmove (ncurses-view-scrwin view) y x)
  (charms/ll:wclrtoeol (ncurses-view-scrwin view)))

(defmethod interface-clear-eob ((implementation (eql :ncurses)) view x y)
  (charms/ll:wmove (ncurses-view-scrwin view) y x)
  (charms/ll:wclrtobot (ncurses-view-scrwin view)))

(defmethod interface-move-cursor ((implementation (eql :ncurses)) view x y)
  (charms/ll:wmove (ncurses-view-scrwin view) y x))

(defmethod interface-redraw-view-after ((implementation (eql :ncurses)) view focus-window-p)
  (let ((attr (attribute-to-bits 'modeline)))
    (charms/ll:attron attr)
    (when (and (ncurses-view-modeline-scrwin view)
               (< 0 (ncurses-view-x view)))
      (charms/ll:move (ncurses-view-y view) (1- (ncurses-view-x view)))
      (charms/ll:vline (char-code #\space) (1+ (ncurses-view-height view))))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh charms/ll:*stdscr*))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:wnoutrefresh (ncurses-view-modeline-scrwin view)))
  (when focus-window-p
    (interface-move-cursor implementation
                           view
                           *cursor-x* *cursor-y*))
  (charms/ll:wnoutrefresh (ncurses-view-scrwin view)))

(defmethod interface-update-display ((implementation (eql :ncurses)))
  (charms/ll:doupdate))
