(defpackage :lem-ncurses/view
  (:use :cl
        :lem
        :lem-ncurses/style
        :lem-core/display)
  (:export :make-view
           :view
           :view-window
           :view-border
           :view-scrwin
           :view-modeline-scrwin
           :view-x
           :view-y
           :view-width
           :view-height
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

(defclass view ()
  ((window :initarg :window :accessor view-window)
   (border :initarg :border :accessor view-border)
   (scrwin :initarg :scrwin :accessor view-scrwin)
   (modeline-scrwin :initarg :modeline-scrwin :accessor view-modeline-scrwin)
   (x :initarg :x :accessor view-x)
   (y :initarg :y :accessor view-y)
   (width :initarg :width :accessor view-width)
   (height :initarg :height :accessor view-height)))

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
     'view
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
  (charms/ll:delwin (view-scrwin view))
  (when (view-modeline-scrwin view)
    (charms/ll:delwin (view-modeline-scrwin view))))

(defun clear (view)
  (charms/ll:clearok (view-scrwin view) 1)
  (when (view-modeline-scrwin view)
    (charms/ll:clearok (view-modeline-scrwin view) 1)))

(defun set-view-size (view width height)
  (setf (view-width view) width)
  (setf (view-height view) height)
  (charms/ll:wresize (view-scrwin view) height width)
  (alexandria:when-let (border (view-border view))
    (destructuring-bind (b-width b-height)
        (compute-border-window-size width height (border-size border))
      (setf (border-width border) b-width
            (border-height border) b-height)
      (charms/ll:wresize (border-win border) b-height b-width)))
  (when (view-modeline-scrwin view)
    (charms/ll:mvwin (view-modeline-scrwin view)
                     (+ (view-y view) height)
                     (view-x view))
    (charms/ll:wresize (view-modeline-scrwin view)
                       1
                       width)))

(defun set-view-pos (view x y)
  (setf (view-x view) x)
  (setf (view-y view) y)
  (charms/ll:mvwin (view-scrwin view) y x)
  (alexandria:when-let (border (view-border view))
    (destructuring-bind (b-x b-y)
        (compute-border-window-position x y (border-size border))
      (charms/ll:mvwin (border-win border) b-y b-x)))
  (when (view-modeline-scrwin view)
    (charms/ll:mvwin (view-modeline-scrwin view)
                     (+ y (view-height view))
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
  (alexandria:when-let (border (view-border view))
    (draw-border border))
  (let ((attr (lem-ncurses/attribute:attribute-to-bits 'modeline-inactive)))
    (charms/ll:attron attr)
    (when (and (view-modeline-scrwin view)
               (< 0 (view-x view)))
      (charms/ll:move (view-y view) (1- (view-x view)))
      (loop :for y :from 0 :to (view-height view)
            :do (charms/ll:mvaddstr (+ (view-y view) y)
                                    (1- (view-x view))
                                    (border-left))))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh charms/ll:*stdscr*))
  (when (view-modeline-scrwin view)
    (charms/ll:wnoutrefresh (view-modeline-scrwin view)))
  (charms/ll:wnoutrefresh (view-scrwin view)))

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
      (set-last-print-cursor (view-window view) x y))
    (print-string scrwin x y string attribute)))

(defmethod draw-object ((object eol-cursor-object) x y view scrwin)
  (set-last-print-cursor (view-window view) x y)
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
  (clear-line (view-scrwin view) x y)
  (%render-line view x y objects (view-scrwin view)))

(defun render-line-on-modeline (view
                                left-objects
                                right-objects
                                default-attribute)
  (print-string (view-modeline-scrwin view)
                0
                0
                (make-string (view-width view)
                             :initial-element #\space)
                default-attribute)
  (%render-line view 0 0 left-objects (view-modeline-scrwin view))
  (render-line-from-behind view
                           0
                           right-objects
                           (view-modeline-scrwin view)))

(defun clear-to-end-of-window (view y)
  (let ((win (view-scrwin view)))
    (when (< y (view-height view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))

(defun update-view (window)
  (let ((scrwin (view-scrwin (window-view window)))
        (cursor-x (last-print-cursor-x window))
        (cursor-y (last-print-cursor-y window)))
    (cond ((covered-with-floating-window-p window cursor-x cursor-y)
           (charms/ll:curs-set 0))
          ((window-cursor-invisible-p window)
           (charms/ll:curs-set 0))
          (t
           (charms/ll:curs-set 1)
           (charms/ll:wmove scrwin cursor-y cursor-x)))
    (charms/ll:wnoutrefresh scrwin)))
