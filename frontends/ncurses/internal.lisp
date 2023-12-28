(defpackage :lem-ncurses/internal
  (:use :cl
        :lem
        :lem-ncurses/style
        :lem-ncurses/key
        :lem-ncurses/view)
  (:export
   ;; ncurses.lisp
   :*terminal-io-saved*
   :escape-delay
   ;; ncurses-pdcurseswin32.lisp
   :input-polling-interval))
(in-package :lem-ncurses/internal)

;; for mouse control
(defparameter *terminal-io-saved* *terminal-io*)

;; escape key delay setting
(define-editor-variable escape-delay 100)

;; popup window margin setting
(setf lem/popup-window::*extra-right-margin* 1)
(setf lem/popup-window::*extra-width-margin* 0)

(define-condition exit (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

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

(defun invoke (function)
  (let ((result nil)
        (input-thread (bt:current-thread)))
    (unwind-protect
         (when (lem-ncurses/term:term-init)
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
      (lem-ncurses/term:term-finalize))
    (when (and (typep result 'exit)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))

(defun get-background-color ()
  (lem-ncurses/term:background-mode))

(defun update-foreground-color (color-name)
  (lem-ncurses/term:term-set-foreground color-name))

(defun update-cursor-shape (cursor-type)
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

(defun update-background-color (color-name)
  (lem-ncurses/term:term-set-background color-name))

(defun get-display-width ()
  (max 5 charms/ll:*cols*))

(defun get-display-height ()
  (max 3 charms/ll:*lines*))

(defun get-char-width ()
  1)

(defun update-display ()
  (lem-ncurses/view:update-view (current-window))
  (charms/ll:doupdate))
