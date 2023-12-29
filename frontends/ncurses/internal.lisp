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

;; popup window margin setting
(setf lem/popup-window::*extra-right-margin* 1)
(setf lem/popup-window::*extra-width-margin* 0)

(define-condition exit (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(defun input-loop (editor-thread)
  (handler-case
      (loop
        (handler-case
            (progn
              (unless (bt:thread-alive-p editor-thread) (return))
              (let ((event (lem-ncurses/input:get-event)))
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

(defun update-background-color (color-name)
  (lem-ncurses/term:term-set-background color-name))

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

(defun get-display-width ()
  (max 5 charms/ll:*cols*))

(defun get-display-height ()
  (max 3 charms/ll:*lines*))

(defun get-char-width ()
  1)

(defun update-display ()
  (update-view (window-view (current-window)))
  (charms/ll:doupdate))
