(defpackage :lem-ncurses/view
  (:use :cl
        :lem
        :lem-ncurses/style
        :lem-core/display)
  (:export :make-view
           :ncurses-view
           :ncurses-view-window
           :ncurses-view-border
           :ncurses-view-scrwin
           :ncurses-view-modeline-scrwin
           :ncurses-view-x
           :ncurses-view-y
           :ncurses-view-width
           :ncurses-view-height
           :border-win
           :border-Width
           :border-height
           :border-size
           :border-shape
           :delete-view
           :clear
           :set-view-size
           :set-view-pos
           :redraw-view-after
           :render-line
           :render-line-on-modeline
           :clear-to-end-of-window
           :update-view))
(in-package :lem-ncurses/view)

(defclass ncurses-view ()
  ((window :initarg :window :accessor ncurses-view-window)
   (border :initarg :border :accessor ncurses-view-border)
   (scrwin :initarg :scrwin :accessor ncurses-view-scrwin)
   (modeline-scrwin :initarg :modeline-scrwin :accessor ncurses-view-modeline-scrwin)
   (x :initarg :x :accessor ncurses-view-x)
   (y :initarg :y :accessor ncurses-view-y)
   (width :initarg :width :accessor ncurses-view-width)
   (height :initarg :height :accessor ncurses-view-height)))

(defstruct border
  win
  width
  height
  size
  (shape nil :type (member nil :drop-curtain)))

(defun compute-border-window-position (x y border-size)
  (let ((x (- x border-size))
        (y (- y border-size)))
    (list x y)))

(defun compute-border-window-size (width height border-size)
  (let ((width (+ width (* border-size 2)))
        (height (+ height (* border-size 2))))
    (list width height)))

(defun make-view (window x y width height use-modeline)
  (flet ((newwin (nlines ncols begin-y begin-x)
           (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
             (when use-modeline (charms/ll:keypad win 1))
             win)))
    (make-instance
     'ncurses-view
     :window window
     :border (when (and (floating-window-p window)
                        (floating-window-border window)
                        (< 0 (floating-window-border window)))
               (destructuring-bind (x y)
                   (compute-border-window-position x
                                                   y
                                                   (floating-window-border window))
                 (destructuring-bind (width height)
                     (compute-border-window-size width
                                                 height
                                                 (floating-window-border window))
                   (let ((win (newwin height width y x)))
                     (make-border :win win
                                  :width width
                                  :height height
                                  :size (floating-window-border window)
                                  :shape (floating-window-border-shape window))))))
     :scrwin (newwin height width y x)
     :modeline-scrwin (when use-modeline (newwin 1 width (+ y height) x))
     :x x
     :y y
     :width width
     :height height)))

(defun delete-view (view)
  (charms/ll:delwin (ncurses-view-scrwin view))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:delwin (ncurses-view-modeline-scrwin view))))

(defun clear (view)
  (charms/ll:clearok (ncurses-view-scrwin view) 1)
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:clearok (ncurses-view-modeline-scrwin view) 1)))

(defun set-view-size (view width height)
  (setf (ncurses-view-width view) width)
  (setf (ncurses-view-height view) height)
  (charms/ll:wresize (ncurses-view-scrwin view) height width)
  (alexandria:when-let (border (ncurses-view-border view))
    (destructuring-bind (b-width b-height)
        (compute-border-window-size width height (border-size border))
      (setf (border-width border) b-width
            (border-height border) b-height)
      (charms/ll:wresize (border-win border) b-height b-width)))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
                     (+ (ncurses-view-y view) height)
                     (ncurses-view-x view))
    (charms/ll:wresize (ncurses-view-modeline-scrwin view)
                       1
                       width)))

(defun set-view-pos (view x y)
  (setf (ncurses-view-x view) x)
  (setf (ncurses-view-y view) y)
  (charms/ll:mvwin (ncurses-view-scrwin view) y x)
  (alexandria:when-let (border (ncurses-view-border view))
    (destructuring-bind (b-x b-y)
        (compute-border-window-position x y (border-size border))
      (charms/ll:mvwin (border-win border) b-y b-x)))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
                     (+ y (ncurses-view-height view))
                     x)))

(defun draw-border (border)
  (let ((win (border-win border))
        (h (1- (border-height border)))
        (w (1- (border-width border)))
        (attr (lem-ncurses/attribute:attribute-to-bits (border-attribute))))
    (charms/ll:wattron win attr)
    (cond ((eq :drop-curtain (border-shape border))
           (charms/ll:mvwaddstr win 0 0 (border-vertical-and-right))
           (charms/ll:mvwaddstr win 0 w (border-vertical-and-left)))
          (t
           (charms/ll:mvwaddstr win 0 0 (border-upleft))
           (charms/ll:mvwaddstr win 0 w (border-upright))))
    (charms/ll:mvwaddstr win h 0 (border-downleft))
    (charms/ll:mvwaddstr win h w (border-downright))
    (loop :for x :from 1 :below w
          :do (charms/ll:mvwaddstr win 0 x (border-up))
              (charms/ll:mvwaddstr win (1- (border-height border)) x (border-down)))
    (loop :for y :from 1 :below h
          :do (charms/ll:mvwaddstr win y 0 (border-left))
              (charms/ll:mvwaddstr win y w (border-right)))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh win)))

(defun redraw-view-after (view)
  (alexandria:when-let (border (ncurses-view-border view))
    (draw-border border))
  (let ((attr (lem-ncurses/attribute:attribute-to-bits 'modeline-inactive)))
    (charms/ll:attron attr)
    (when (and (ncurses-view-modeline-scrwin view)
               (< 0 (ncurses-view-x view)))
      (charms/ll:move (ncurses-view-y view) (1- (ncurses-view-x view)))
      (loop :for y :from 0 :to (ncurses-view-height view)
            :do (charms/ll:mvaddstr (+ (ncurses-view-y view) y)
                                    (1- (ncurses-view-x view))
                                    (border-left))))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh charms/ll:*stdscr*))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:wnoutrefresh (ncurses-view-modeline-scrwin view)))
  (charms/ll:wnoutrefresh (ncurses-view-scrwin view)))

;;;
(defun print-string (scrwin x y string attribute)
  (let ((attr (lem-ncurses/attribute:attribute-to-bits attribute)))
    (charms/ll:wattron scrwin attr)
    (charms/ll:mvwaddstr scrwin y x string)
    (charms/ll:wattroff scrwin attr)))

(defgeneric draw-object (object x y view scrwin))

(defmethod draw-object ((object void-object) x y view scrwin)
  (values))

(defmethod draw-object ((object text-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor (ncurses-view-window view) x y))
    (print-string scrwin x y string attribute)))

(defmethod draw-object ((object eol-cursor-object) x y view scrwin)
  (lem-core::set-last-print-cursor (ncurses-view-window view) x y)
  (print-string
   scrwin
   x
   y
   " "
   (lem:make-attribute :foreground
                       (lem:color-to-hex-string (eol-cursor-object-color object)))))

(defmethod draw-object ((object extend-to-eol-object) x y view scrwin)
  (let ((width (lem-if:view-width (lem-core:implementation) view)))
    (when (< x width)
      (print-string
       scrwin
       x
       y
       (make-string (- width x) :initial-element #\space)
       (lem:make-attribute :background
                           (lem:color-to-hex-string (extend-to-eol-object-color object)))))))

(defmethod draw-object ((object line-end-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (print-string
     scrwin
     (+ x (line-end-object-offset object))
     y
     string
     attribute)))

(defmethod draw-object ((object image-object) x y view scrwin)
  (values))

(defun render-line-from-behind (view y objects scrwin)
  (loop :with current-x := (lem-if:view-width (lem-core:implementation) view)
        :for object :in objects
        :do (decf current-x (lem-ncurses/drawing-object:object-width object))
            (draw-object object current-x y view scrwin)))

(defun clear-line (scrwin x y)
  (charms/ll:wmove scrwin y x)
  (charms/ll:wclrtoeol scrwin))

(defun %render-line (view x y objects scrwin)
  (loop :for object :in objects
        :do (draw-object object x y view scrwin)
            (incf x (lem-ncurses/drawing-object:object-width object))))

(defun render-line (view x y objects)
  (clear-line (ncurses-view-scrwin view) x y)
  (%render-line view x y objects (ncurses-view-scrwin view)))

(defun render-line-on-modeline (view
                                left-objects
                                right-objects
                                default-attribute)
  (print-string (ncurses-view-modeline-scrwin view)
                0
                0
                (make-string (ncurses-view-width view)
                             :initial-element #\space)
                default-attribute)
  (%render-line view 0 0 left-objects (ncurses-view-modeline-scrwin view))
  (render-line-from-behind view
                           0
                           right-objects
                           (ncurses-view-modeline-scrwin view)))

(defun clear-to-end-of-window (view y)
  (let ((win (ncurses-view-scrwin view)))
    (when (< y (ncurses-view-height view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))

(defun update-view (window)
  (let ((scrwin (ncurses-view-scrwin (window-view window))))
    (let ((cursor-x (last-print-cursor-x window))
          (cursor-y (last-print-cursor-y window)))
      (cond ((covered-with-floating-window-p window cursor-x cursor-y)
             (charms/ll:curs-set 0))
            ((window-cursor-invisible-p window)
             (charms/ll:curs-set 0))
            (t
             (charms/ll:curs-set 1)
             (charms/ll:wmove scrwin cursor-y cursor-x))))
    (charms/ll:wnoutrefresh scrwin)))
