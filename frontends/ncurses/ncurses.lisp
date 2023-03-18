(defpackage :lem-ncurses
  (:use :cl
        :lem
        :lem-ncurses/style)
  (:export ;; ncurses.lisp
           :escape-delay
           ;; ncurses-pdcurseswin32.lisp
           :input-polling-interval))
(in-package :lem-ncurses)

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
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(define-condition exit-editor (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(defvar *keycode-table* (make-hash-table))
(defvar *keyname-table* (make-hash-table :test 'equal))

(defun defkeycode (name code &optional key)
  (setf (gethash name *keyname-table*) code)
  (when key (setf (gethash code *keycode-table*) key)))

(defun get-code (name)
  (let ((code (gethash name *keyname-table*)))
    (assert code)
    code))

(defun char-to-key (char)
  (or (gethash (char-code char) *keycode-table*)
      (make-key :sym (string char))))

(defun get-key-from-name (name)
  (char-to-key (code-char (get-code name))))

(defkeycode "C-@" 0 (make-key :ctrl t :sym "@"))
(defkeycode "C-a" 1 (make-key :ctrl t :sym "a"))
(defkeycode "C-b" 2 (make-key :ctrl t :sym "b"))
(defkeycode "C-c" 3 (make-key :ctrl t :sym "c"))
(defkeycode "C-d" 4 (make-key :ctrl t :sym "d"))
(defkeycode "C-e" 5 (make-key :ctrl t :sym "e"))
(defkeycode "C-f" 6 (make-key :ctrl t :sym "f"))
(defkeycode "C-g" 7 (make-key :ctrl t :sym "g"))
(defkeycode "C-h" 8 (make-key :ctrl t :sym "h"))
(defkeycode "C-i" 9 (make-key :sym "Tab"))
(defkeycode "C-j" 10 (make-key :ctrl t :sym "j"))
(defkeycode "C-k" 11 (make-key :ctrl t :sym "k"))
(defkeycode "C-l" 12 (make-key :ctrl t :sym "l"))
(defkeycode "C-m" 13 (make-key :sym "Return"))
(defkeycode "C-n" 14 (make-key :ctrl t :sym "n"))
(defkeycode "C-o" 15 (make-key :ctrl t :sym "o"))
(defkeycode "C-p" 16 (make-key :ctrl t :sym "p"))
(defkeycode "C-q" 17 (make-key :ctrl t :sym "q"))
(defkeycode "C-r" 18 (make-key :ctrl t :sym "r"))
(defkeycode "C-s" 19 (make-key :ctrl t :sym "s"))
(defkeycode "C-t" 20 (make-key :ctrl t :sym "t"))
(defkeycode "C-u" 21 (make-key :ctrl t :sym "u"))
(defkeycode "C-v" 22 (make-key :ctrl t :sym "v"))
(defkeycode "C-w" 23 (make-key :ctrl t :sym "w"))
(defkeycode "C-x" 24 (make-key :ctrl t :sym "x"))
(defkeycode "C-y" 25 (make-key :ctrl t :sym "y"))
(defkeycode "C-z" 26 (make-key :ctrl t :sym "z"))
(defkeycode "escape" 27 (make-key :sym "Escape"))
(defkeycode "C-\\" 28 (make-key :ctrl t :sym "\\"))
(defkeycode "C-]" 29 (make-key :ctrl t :sym "]"))
(defkeycode "C-^" 30 (make-key :ctrl t :sym "^"))
(defkeycode "C-_" 31 (make-key :ctrl t :sym "_"))
(defkeycode "Spc" #x20 (make-key :sym "Space"))
(defkeycode "[backspace]" #x7F (make-key :sym "Backspace"))

(loop :for code :from #x21 :below #x7F
      :do (let ((string (string (code-char code))))
            (defkeycode string code (make-key :sym string))))

(defkeycode "[down]" #o402 (make-key :sym "Down"))
(defkeycode "[up]" #o403 (make-key :sym "Up"))
(defkeycode "[left]" #o404 (make-key :sym "Left"))
(defkeycode "[right]" #o405 (make-key :sym "Right"))
(defkeycode "C-down" 525 (make-key :ctrl t :sym "Down"))
(defkeycode "C-down_en" 526 (make-key :ctrl t :sym "Down"))
(defkeycode "M-up" 564 (make-key :meta t :sym "Up"))
(defkeycode "M-up_en" 565 (make-key :meta t :sym "Up"))
(defkeycode "M-down_en" 524 (make-key :meta t :sym "Down"))
(defkeycode "M-left_en" 544 (make-key :meta t :sym "Left"))
(defkeycode "M-right_en" 559 (make-key :meta t :sym "Right"))
(defkeycode "C-up" 566 (make-key :ctrl t :sym "Up"))
(defkeycode "C-up_en" 567 (make-key :ctrl t :sym "Up"))
(defkeycode "C-left" 545 (make-key :ctrl t :sym "Left"))
(defkeycode "C-left_en" 546 (make-key :ctrl t :sym "Left"))
(defkeycode "C-right" 560 (make-key :ctrl t :sym "Right"))
(defkeycode "C-right_en" 561 (make-key :ctrl t :sym "Right"))
(defkeycode "[home]" #o406 (make-key :sym "Home"))
(defkeycode "[backspace]" #o407 (make-key :sym "Backspace"))
(defkeycode "[f0]" #o410 (make-key :sym "F0"))
(defkeycode "[f1]" #o411 (make-key :sym "F1"))
(defkeycode "[f2]" #o412 (make-key :sym "F2"))
(defkeycode "[f3]" #o413 (make-key :sym "F3"))
(defkeycode "[f4]" #o414 (make-key :sym "F4"))
(defkeycode "[f5]" #o415 (make-key :sym "F5"))
(defkeycode "[f6]" #o416 (make-key :sym "F6"))
(defkeycode "[f7]" #o417 (make-key :sym "F7"))
(defkeycode "[f8]" #o420 (make-key :sym "F8"))
(defkeycode "[f9]" #o421 (make-key :sym "F9"))
(defkeycode "[f10]" #o422 (make-key :sym "F10"))
(defkeycode "[f11]" #o423 (make-key :sym "F11"))
(defkeycode "[f12]" #o424 (make-key :sym "F12"))
(defkeycode "[sf1]" #o425 (make-key :shift t :sym "F1"))
(defkeycode "[sf2]" #o426 (make-key :shift t :sym "F2"))
(defkeycode "[sf3]" #o427 (make-key :shift t :sym "F3"))
(defkeycode "[sf4]" #o430 (make-key :shift t :sym "F4"))
(defkeycode "[sf5]" #o431 (make-key :shift t :sym "F5"))
(defkeycode "[sf6]" #o432 (make-key :shift t :sym "F6"))
(defkeycode "[sf7]" #o433 (make-key :shift t :sym "F7"))
(defkeycode "[sf8]" #o434 (make-key :shift t :sym "F8"))
(defkeycode "[sf9]" #o435 (make-key :shift t :sym "F9"))
(defkeycode "[sf10]" #o436 (make-key :shift t :sym "F10"))
(defkeycode "[sf11]" #o437 (make-key :shift t :sym "F11"))
(defkeycode "[sf12]" #o440 (make-key :shift t :sym "F12"))
(defkeycode "[dl]" #o510)
(defkeycode "[il]" #o511)
(defkeycode "[dc]" #o512 (make-key :sym "Delete"))
(defkeycode "C-dc" 519 (make-key :ctrl t :sym "Delete"))
(defkeycode "[ic]" #o513)
(defkeycode "[eic]" #o514)
(defkeycode "[clear]" #o515)
(defkeycode "[eos]" #o516)
(defkeycode "[eol]" #o517)
(defkeycode "[sf]" #o520 (make-key :shift t :sym "Down"))
(defkeycode "[sr]" #o521 (make-key :shift t :sym "Up"))
(defkeycode "[npage]" #o522 (make-key :sym "PageDown"))
(defkeycode "[ppage]" #o523 (make-key :sym "PageUp"))
(defkeycode "[stab]" #o524)
(defkeycode "[ctab]" #o525)
(defkeycode "[catab]" #o526)
(defkeycode "[enter]" #o527)
(defkeycode "[print]" #o532)
(defkeycode "[ll]" #o533)
(defkeycode "[a1]" #o534)
(defkeycode "[a3]" #o535)
(defkeycode "[b2]" #o536)
(defkeycode "[c1]" #o537)
(defkeycode "[c3]" #o540)
(defkeycode "[btab]" #o541  (make-key :shift t :sym "Tab"))
(defkeycode "[beg]" #o542)
(defkeycode "[cancel]" #o543)
(defkeycode "[close]" #o544)
(defkeycode "[command]" #o545)
(defkeycode "[copy]" #o546)
(defkeycode "[create]" #o547)
(defkeycode "[end]" #o550 (make-key :sym "End"))
(defkeycode "[exit]" #o551)
(defkeycode "[find]" #o552)
(defkeycode "[help]" #o553)
(defkeycode "[mark]" #o554)
(defkeycode "[message]" #o555)
(defkeycode "[move]" #o556)
(defkeycode "[next]" #o557)
(defkeycode "[open]" #o560)
(defkeycode "[options]" #o561)
(defkeycode "[previous]" #o562)
(defkeycode "[redo]" #o563)
(defkeycode "[reference]" #o564)
(defkeycode "[refresh]" #o565)
(defkeycode "[replace]" #o566)
(defkeycode "[restart]" #o567)
(defkeycode "[resume]" #o570)
(defkeycode "[save]" #o571)
(defkeycode "[sbeg]" #o572)
(defkeycode "[scancel]" #o573)
(defkeycode "[scommand]" #o574)
(defkeycode "[scopy]" #o575)
(defkeycode "[screate]" #o576)
(defkeycode "[sdc]" #o577 (make-key :shift t :sym "Delete"))
(defkeycode "[sdl]" #o600)
(defkeycode "[select]" #o601)
(defkeycode "[send]" #o602 (make-key :shift t :sym "End"))
(defkeycode "[seol]" #o603)
(defkeycode "[sexit]" #o604)
(defkeycode "[sfind]" #o605)
(defkeycode "[shelp]" #o606)
(defkeycode "[shome]" #o607 (make-key :shift t :sym "Home"))
(defkeycode "[sic]" #o610)
(defkeycode "[sleft]" #o611 (make-key :shift t :sym "Left"))
(defkeycode "[smessage]" #o612)
(defkeycode "[smove]" #o613)
(defkeycode "[snext]" #o614 (make-key :shift t :sym "PageDown"))
(defkeycode "[soptions]" #o615)
(defkeycode "[sprevious]" #o616 (make-key :shift t :sym "PageUp"))
(defkeycode "[sprint]" #o617)
(defkeycode "[sredo]" #o620)
(defkeycode "[sreplace]" #o621)
(defkeycode "[sright]" #o622 (make-key :shift t :sym "Right"))
(defkeycode "[srsume]" #o623)
(defkeycode "[ssave]" #o624)
(defkeycode "[ssuspend]" #o625)
(defkeycode "[sundo]" #o626)
(defkeycode "[suspend]" #o627)
(defkeycode "[undo]" #o630)
(defkeycode "[mouse]" #o631)
(defkeycode "[resize]" #o632)
(defkeycode "[event]" #o633)

(defstruct border
  win
  width
  height
  size)

(defstruct ncurses-view
  border
  scrwin
  modeline-scrwin
  x
  y
  width
  height)

(defun compute-attribute-value (attribute cursorp)
  (let* ((foreground (attribute-foreground attribute))
         (background (or (attribute-background attribute)
                         lem-if:*background-color-of-drawing-window*))
         (bits (logior (if (or cursorp (lem::attribute-reverse-p attribute))
                           (lem.term:get-color-pair background foreground)
                           (lem.term:get-color-pair foreground background))
                       0
                       (if (lem::attribute-bold-p attribute)
                           charms/ll:a_bold
                           0)
                       (if (lem::attribute-underline-p attribute)
                           charms/ll:a_underline
                           0))))
    bits))

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (ensure-attribute attribute-or-name nil))
        (cursorp (eq attribute-or-name 'cursor)))
    (when (and lem-if:*background-color-of-drawing-window* (null attribute))
      (setf attribute (make-attribute :background lem-if:*background-color-of-drawing-window*)))
    (if (null attribute)
        0
        (cond ((lem::get-attribute-cache
                attribute
                :background lem-if:*background-color-of-drawing-window*))
              (t
               (let ((bits (compute-attribute-value attribute cursorp)))
                 (setf (lem::get-attribute-cache
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
    (exit-editor (c) (return-from input-loop c))))

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
                                 (lambda () (error 'exit-editor :value report)))))))
               (setf result (input-loop editor-thread)))))
      (lem.term:term-finalize))
    (when (and (typep result 'exit-editor)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))

(defmethod lem-if:get-background-color ((implementation ncurses))
  (lem.term:background-mode))

(defmethod lem-if:update-foreground ((implementation ncurses) color-name)
  (lem.term:term-set-foreground color-name))

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
                                  :size (floating-window-border window))))))
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
    (charms/ll:mvwaddstr win 0 0 (border-upleft))
    (charms/ll:mvwaddstr win 0 w (border-upright))
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
      (if (lem::covered-with-floating-window-p (current-window) cursor-x cursor-y)
          (charms/ll:curs-set 0)
          (progn
            (charms/ll:curs-set 1)
            (charms/ll:wmove scrwin cursor-y cursor-x))))
    (charms/ll:wnoutrefresh scrwin)
    (charms/ll:doupdate)))

(defmethod lem-if:scroll ((implementation ncurses) view n)
  (charms/ll:wscrl (ncurses-view-scrwin view) n))

(defmethod lem-if:clipboard-paste ((implementation ncurses))
  (lem-ncurses.clipboard:paste))

(defmethod lem-if:clipboard-copy ((implementation ncurses) text)
  (lem-ncurses.clipboard:copy text))

(pushnew :lem-ncurses *features*)
