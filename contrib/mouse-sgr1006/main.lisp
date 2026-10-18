(uiop/package:define-package :lem-mouse-sgr1006/main
  (:nicknames :lem-mouse-sgr1006) (:use :cl :lem)
  (:shadow) (:export :parse-mouse-event
                     :*scroll-unfocused-window*)
  (:intern))
(in-package :lem-mouse-sgr1006/main)
;;;don't edit above
(defparameter *message-on-mouse-event* nil)
(defparameter *scroll-unfocused-window* t
  "Kept for compatibility: the wheel always scrolls the window under the pointer.")

;; mouse button type
(defvar *mouse-button-1*   0)
(defvar *mouse-wheel-up*   64)
(defvar *mouse-wheel-down* 65)

(defun parse-mouse-event (getch-fn)
  (let ((msg (loop :for c := (code-char (funcall getch-fn))
                   :with result
                   :with part
                   :until (or (char= c #\m)
                              (char= c #\M))
                   :when (char= c #\;)
                   :do (setq result #1=(cons (parse-integer (format nil "~{~A~}"
                                                                    (reverse part)))
                                             result)
                             part nil)
                   :else
                   :do (push c part)
                   :finally (return (cons c (reverse #1#))))))
    (lambda ()
      (multiple-value-bind (bstate btype x1 y1)
          (apply #'values msg)
        ;; convert mouse position from 1-origin to 0-origin
        (decf x1)
        (decf y1)
        ;; a dummy key event exits isearch-mode before the mouse acts
        (when (or (and (eql btype *mouse-button-1*) (eql bstate #\M))
                  (member btype (list *mouse-wheel-up* *mouse-wheel-down*)))
          (lem:send-event (lem:make-key :sym "NopKey")))
        (dispatch-mouse-event bstate btype x1 y1)))))

;;; Events go through Lem's core mouse handling, as in the SDL2 frontend, so a terminal
;;; click does what a GUI click does: clickable text (filer items, tab bar, buttons)
;;; runs its callback, separators drag, double and triple clicks select, the wheel
;;; scrolls the window under the pointer.

(defvar *mouse-motion-flag* 32)
(defvar *double-click-interval* 0.4 "Seconds within which a second click counts as a double click.")
(defvar *last-click* nil "(time x y clicks) of the previous button-1 press.")

(defun sgr-button (btype)
  (case (logand btype 3)
    (0 :button-1)
    (1 :button-2)
    (2 :button-3)))

(defun click-count (x y)
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (destructuring-bind (&optional time last-x last-y (clicks 0)) *last-click*
      (let ((count (if (and time (= x last-x) (= y last-y)
                            (< (- now time) *double-click-interval*))
                       (1+ clicks)
                       1)))
        (setf *last-click* (list now x y count))
        count))))

(defun dispatch-mouse-event (bstate btype x y)
  (cond ((eql btype *mouse-wheel-up*)
         (when (eql bstate #\M)
           (lem:receive-mouse-wheel x y x y 0 1)))
        ((eql btype *mouse-wheel-down*)
         (when (eql bstate #\M)
           (lem:receive-mouse-wheel x y x y 0 -1)))
        ((logtest btype *mouse-motion-flag*)
         (lem:receive-mouse-motion x y x y (sgr-button btype)))
        ((null (sgr-button btype)) nil)
        ((eql bstate #\M)
         (let ((button (sgr-button btype)))
           (lem:receive-mouse-button-down x y x y button
                                          (if (eq button :button-1) (click-count x y) 1))))
        ((eql bstate #\m)
         (lem:receive-mouse-button-up x y x y (sgr-button btype))))
  (when *message-on-mouse-event*
    (lem:send-event (lambda () (lem:message "mouse:~s ~s ~s ~s" bstate btype x y)))))

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (format lem-ncurses:*terminal-io-saved* "~A[?1000h~A[?1002h~A[?1006h~%" #\esc #\esc #\esc)
  (ignore-errors
    (dolist (window (lem:window-list))
      (lem:screen-clear (lem:window-screen window)))
    (lem:redraw-display))
  (lem:run-hooks *enable-hook*))

(defun disable-hook ()
  (format lem-ncurses:*terminal-io-saved* "~A[?1006l~A[?1002l~A[?1000l~%" #\esc #\esc #\esc)
  (ignore-errors
    (dolist (window (lem:window-list))
      (lem:screen-clear (lem:window-screen window)))
    (lem:redraw-display))
  (run-hooks *disable-hook*))

(define-minor-mode mouse-sgr-1006-mode
    (:global t
     :enable-hook #'enable-hook
     :disable-hook #'disable-hook))

(defun enable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode t))

(defun disable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode nil))

(add-hook *after-init-hook* 'enable-mouse-sgr-1006-mode)
(add-hook *exit-editor-hook* 'disable-mouse-sgr-1006-mode)

(eval-when (:load-toplevel)
  (enable-mouse-sgr-1006-mode))
