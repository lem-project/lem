(defpackage :lem-ncurses
  (:use :cl
        :lem
        :lem-ncurses/style
        :lem-ncurses/key)
  (:export
   ;; ncurses.lisp
   :*terminal-io-saved*
   :escape-delay
   ;; ncurses-pdcurseswin32.lisp
   :input-polling-interval))
(in-package :lem-ncurses)

;; for mouse control
(defparameter *terminal-io-saved* *terminal-io*)

;; escape key delay setting
(define-editor-variable escape-delay 100)

;; popup window margin setting
(setf lem/popup-window::*extra-right-margin* 1)
(setf lem/popup-window::*extra-width-margin* 0)

;; prompt window margin setting
;; (setf lem/prompt-window::*extra-side-margin* 2)


(defclass ncurses (lem:implementation)
  ()
  (:default-initargs
   :name :ncurses
   :redraw-after-modifying-floating-window t))

(define-condition exit (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(defstruct border
  win
  width
  height
  size
  (shape nil :type (member nil :drop-curtain)))

(defstruct ncurses-view
  border
  scrwin
  modeline-scrwin
  x
  y
  width
  height)

(defun underline-color (attribute)
  (cond ((eq t (attribute-underline attribute))
         nil)
        ((and (attribute-underline attribute)
              (parse-color (attribute-underline attribute)))
         (attribute-underline attribute))
        (t
         nil)))

(defun compute-attribute-value (attribute cursorp)
  (let* ((underline-color (underline-color attribute))
         (foreground (or underline-color (attribute-foreground attribute)))
         (background (or (attribute-background attribute)
                         lem-if:*background-color-of-drawing-window*))
         (bits (logior (lem.term:get-color-pair foreground background)
                       0
                       (if (attribute-bold attribute)
                           charms/ll:a_bold
                           0)
                       (if (attribute-underline attribute)
                           charms/ll:a_underline
                           0)
                       (if (or cursorp (attribute-reverse attribute))
                           charms/ll:a_reverse
                           0))))
    bits))

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (ensure-attribute attribute-or-name nil))
        (cursorp (eq attribute-or-name 'cursor)))
    (when (and lem-if:*background-color-of-drawing-window* (null attribute))
      (setf attribute (make-attribute :background lem-if:*background-color-of-drawing-window*)))
    (if (null attribute)
        0
        (cond ((get-attribute-cache
                attribute
                :background lem-if:*background-color-of-drawing-window*))
              (t
               (let ((bits (compute-attribute-value attribute cursorp)))
                 (setf (get-attribute-cache
                        attribute
                        :background lem-if:*background-color-of-drawing-window*)
                       bits)
                 bits))))))

;; for input
;;  (we don't use stdscr for input because it calls wrefresh implicitly
;;   and causes the display confliction by two threads)
(defvar *padwin* nil)
(defun getch ()
  (unless *padwin*
    (setf *padwin* (charms/ll:newpad 1 1))
    (charms/ll:keypad *padwin* 1)
    (charms/ll:wtimeout *padwin* -1))
  (charms/ll:wgetch *padwin*))
(defmacro with-getch-input-timeout ((time) &body body)
  `(progn
     (charms/ll:wtimeout *padwin* ,time)
     (unwind-protect (progn ,@body)
       (charms/ll:wtimeout *padwin* -1))))

(defun get-key (code)
  (let* ((char (let ((nbytes (utf8-bytes code)))
                 (if (= nbytes 1)
                   (code-char code)
                   (let ((vec (make-array nbytes :element-type '(unsigned-byte 8))))
                     (setf (aref vec 0) code)
                     (with-getch-input-timeout (100)
                       (loop :for i :from 1 :below nbytes
                             :do (setf (aref vec i) (getch))))
                     (handler-case (schar (babel:octets-to-string vec) 0)
                       (babel-encodings:invalid-utf8-continuation-byte ()
                         (code-char code)))))))
         (key (char-to-key char)))
    key))

(defun csi\[1 ()
  (or (case (getch)
        (#.(char-code #\;)
           (case (getch)
             (#.(char-code #\2)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :sym "Home"))))
             (#.(char-code #\3)
                (case (getch)
                  (#.(char-code #\A) (make-key :meta t :sym "Up"))
                  (#.(char-code #\B) (make-key :meta t :sym "Down"))
                  (#.(char-code #\C) (make-key :meta t :sym "Right"))
                  (#.(char-code #\D) (make-key :meta t :sym "Left"))
                  (#.(char-code #\F) (make-key :meta t :sym "End"))
                  (#.(char-code #\H) (make-key :meta t :sym "Home"))))
             (#.(char-code #\4)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :meta t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :meta t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :meta t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :meta t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :meta t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :meta t :sym "Home"))))
             (#.(char-code #\5)
                (case (getch)
                  (#.(char-code #\A) (make-key :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :ctrl t :sym "Home"))))
             (#.(char-code #\6)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :ctrl t :sym "Home"))))
             (#.(char-code #\7)
                (case (getch)
                  (#.(char-code #\A) (make-key :meta t :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :meta t :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :meta t :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :meta t :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :meta t :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :meta t :ctrl t :sym "Home"))))
             (#.(char-code #\8)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :meta t :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :meta t :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :meta t :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :meta t :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :meta t :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :meta t :ctrl t :sym "Home")))))))
      (get-key-from-name "escape")))

(let ((resize-code (get-code "[resize]"))
      (abort-code (get-code "C-]"))
      (escape-code (get-code "escape")))
  (defun get-event ()
    (tagbody :start
      (return-from get-event
        (let ((code (getch)))
          (cond ((= code -1) (go :start))
                ((= code resize-code) :resize)
                ((= code abort-code) :abort)
                ((= code escape-code)
                 (let ((code (with-getch-input-timeout
                                 ((variable-value 'escape-delay))
                               (getch))))
                   (cond ((= code -1)
                          (get-key-from-name "escape"))
                         ((= code #.(char-code #\[))
                          (with-getch-input-timeout (100)
                            (case (getch)
                              (#.(char-code #\<)
                                 ;;sgr(1006)
                                 (uiop:symbol-call :lem-mouse-sgr1006
                                                   :parse-mouse-event
                                                   #'getch))
                              (#.(char-code #\1)
                                 (csi\[1))
                              (t (get-key-from-name "escape")))))
                         (t
                          (let ((key (get-key code)))
                            (make-key :meta t
                                      :sym (key-sym key)
                                      :ctrl (key-ctrl key)))))))
                (t
                 (get-key code))))))))

(defun input-loop (editor-thread)
  (handler-case
      (loop
        (handler-case
            (progn
              (unless (bt:thread-alive-p editor-thread) (return))
              (let ((event (get-event)))
                (if (eq event :abort)
                    (send-abort-event editor-thread nil)
                    (send-event event))))
          #+sbcl
          (sb-sys:interactive-interrupt (c)
            (declare (ignore c))
            (send-abort-event editor-thread t))))
    (exit (c) (return-from input-loop c))))

(defmethod lem-if:invoke ((implementation ncurses) function)
  (let ((result nil)
        (input-thread (bt:current-thread)))
    (unwind-protect
         (when (lem.term:term-init)
           (let ((*standard-output* (make-broadcast-stream))
                 (*error-output* (make-broadcast-stream))
                 (*terminal-io* (make-broadcast-stream)))
             (let ((editor-thread
                     (funcall function
                              nil
                              (lambda (report)
                                (bt:interrupt-thread
                                 input-thread
                                 (lambda () (error 'exit :value report)))))))
               (setf result (input-loop editor-thread)))))
      (lem.term:term-finalize))
    (when (and (typep result 'exit)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))

(defmethod lem-if:get-background-color ((implementation ncurses))
  (lem.term:background-mode))

(defmethod lem-if:update-foreground ((implementation ncurses) color-name)
  (lem.term:term-set-foreground color-name))

(defmethod lem-if:update-cursor-shape ((implementation ncurses) cursor-type)
  (uiop:run-program `("printf"
                      ,(format nil "~C[~D q"
                               #\Esc
                               (case cursor-type
                                 (:box 2)
                                 (:bar 5)
                                 (:underline 4)
                                 (otherwise 2))))
                    :output :interactive
                    :ignore-error-status t))

(defmethod lem-if:update-background ((implementation ncurses) color-name)
  (lem.term:term-set-background color-name))

(defmethod lem-if:display-width ((implementation ncurses))
  (max 5 charms/ll:*cols*))

(defmethod lem-if:display-height ((implementation ncurses))
  (max 3 charms/ll:*lines*))

(defun compute-border-window-size (width height border-size)
  (let ((width (+ width (* border-size 2)))
        (height (+ height (* border-size 2))))
    (list width height)))

(defun compute-border-window-position (x y border-size)
  (let ((x (- x border-size))
        (y (- y border-size)))
    (list x y)))

(defmethod lem-if:make-view
    ((implementation ncurses) window x y width height use-modeline)
  (flet ((newwin (nlines ncols begin-y begin-x)
           (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
             (when use-modeline (charms/ll:keypad win 1))
             win)))
    (make-ncurses-view
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

(defmethod lem-if:delete-view ((implementation ncurses) view)
  (charms/ll:delwin (ncurses-view-scrwin view))
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:delwin (ncurses-view-modeline-scrwin view))))

(defmethod lem-if:clear ((implementation ncurses) view)
  (charms/ll:clearok (ncurses-view-scrwin view) 1)
  (when (ncurses-view-modeline-scrwin view)
    (charms/ll:clearok (ncurses-view-modeline-scrwin view) 1)))

(defmethod lem-if:set-view-size ((implementation ncurses) view width height)
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

(defmethod lem-if:set-view-pos ((implementation ncurses) view x y)
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

(defmethod lem-if:print ((implementation ncurses) view x y string attribute)
  (let ((attr (attribute-to-bits attribute)))
    (charms/ll:wattron (ncurses-view-scrwin view) attr)
    ;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
    (charms/ll:mvwaddstr (ncurses-view-scrwin view) y x string)
    ;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
    (charms/ll:wattroff (ncurses-view-scrwin view) attr)))

(defmethod lem-if:print-modeline ((implementation ncurses) view x y string attribute)
  (let ((attr (attribute-to-bits attribute)))
    (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
    (charms/ll:mvwaddstr (ncurses-view-modeline-scrwin view) y x string)
    (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)))

(defmethod lem-if:clear-eol ((implementation ncurses) view x y)
  (cond (lem-if:*background-color-of-drawing-window*
         (let ((attr (attribute-to-bits (make-attribute :background lem-if:*background-color-of-drawing-window*))))
           (charms/ll:wattron (ncurses-view-scrwin view) attr)
           (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                                y
                                x
                                (make-string (- (ncurses-view-width view) x) :initial-element #\space))
           (charms/ll:wattroff (ncurses-view-scrwin view) attr)))
        (t
         (charms/ll:wmove (ncurses-view-scrwin view) y x)
         (charms/ll:wclrtoeol (ncurses-view-scrwin view)))))

(defmethod lem-if:clear-eob ((implementation ncurses) view x y)
  (cond (lem-if:*background-color-of-drawing-window*
         (let ((attr (attribute-to-bits (make-attribute :background lem-if:*background-color-of-drawing-window*))))
           (charms/ll:wattron (ncurses-view-scrwin view) attr)
           (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                                y
                                x
                                (make-string (- (ncurses-view-width view) x) :initial-element #\space))
           (loop :for y1 :from y :to (ncurses-view-height view)
                 :do (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                                          y1
                                          0
                                          (make-string (ncurses-view-width view) :initial-element #\space)))
           (charms/ll:wattroff (ncurses-view-scrwin view) attr)))
        (t
         (charms/ll:wmove (ncurses-view-scrwin view) y x)
         (charms/ll:wclrtobot (ncurses-view-scrwin view)))))

(defun draw-border (border)
  (let ((win (border-win border))
        (h (1- (border-height border)))
        (w (1- (border-width border)))
        (attr (attribute-to-bits (border-attribute))))
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

(defmethod lem-if:redraw-view-after ((implementation ncurses) view)
  (alexandria:when-let (border (ncurses-view-border view))
    (draw-border border))
  (let ((attr (attribute-to-bits 'modeline-inactive)))
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

(defmethod lem-if:update-display ((implementation ncurses))
  (let ((scrwin (ncurses-view-scrwin (window-view (current-window)))))
    (let ((cursor-x (last-print-cursor-x (current-window)))
          (cursor-y (last-print-cursor-y (current-window))))
      (cond ((covered-with-floating-window-p (current-window) cursor-x cursor-y)
             (charms/ll:curs-set 0))
            ((window-cursor-invisible-p (current-window))
             (charms/ll:curs-set 0))
            (t
             (charms/ll:curs-set 1)
             (charms/ll:wmove scrwin cursor-y cursor-x))))
    (charms/ll:wnoutrefresh scrwin)
    (charms/ll:doupdate)))

(defmethod lem-if:force-update-view ((implementation ncurses) view)
  (charms/ll:redrawwin (ncurses-view-scrwin view)))

(defmethod lem-if:clipboard-paste ((implementation ncurses))
  (lem-ncurses.clipboard:paste))

(defmethod lem-if:clipboard-copy ((implementation ncurses) text)
  (lem-ncurses.clipboard:copy text))

(pushnew :lem-ncurses *features*)
