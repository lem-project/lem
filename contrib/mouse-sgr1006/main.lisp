(uiop/package:define-package :lem-mouse-sgr1006/main
  (:nicknames :lem-mouse-sgr1006) (:use :cl :lem)
  (:shadow) (:export :parse-mouse-event) (:intern))
(in-package :lem-mouse-sgr1006/main)
;;;don't edit above
(defparameter *message-on-mouse-event* nil)

(defvar *dragging-window* ())

(defun move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point) x))

(defun parse-mouse-event ()
  (let ((msg (loop :for c := (prog1 (code-char (charms/ll:getch))
                               (charms/ll:timeout -1))
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
      (when (zerop (second msg))
        (cond ((and (eql (second msg) 0)
                    (eql (first msg) #\M)) ;; button-1 down
               (find-if (lambda(o)
                          (let ((x (lem:window-x o))
                                (w (lem:window-width o))
                                (y (lem:window-y o))
                                (h (lem:window-height o)))
                            (or
                             (and (< x (third msg) (+ 1 x w))
                                  (= y (fourth msg))
                                  (setf *dragging-window* (list o 'y)))
                             (and (= x (third msg))
                                  (< y (fourth msg) (+ -1 y h))
                                  (setf *dragging-window* (list o 'x)))
                             (and (< x (third msg) (+ 1 x w))
                                  (< y (fourth msg) (+ -1 y h))
                                  (lem:send-event
                                   (lambda ()
                                     (setf (lem:current-window) o)
                                     (move-to-cursor o
                                                     (- (third msg) x 1)
                                                     (- (fourth msg) y 1))
                                     (lem:redraw-display)))))))
                        (lem:window-list)))
              ((and (eql (second msg) 0)  ;; button-1 up
                    (eql (first msg) #\m))
               (when (windowp (first *dragging-window*))
                 (if (eql (second *dragging-window*) 'x)
                     (lem:shrink-window-horizontally
                      (- (lem:window-x (first *dragging-window*))
                         (first (cddr msg))))
                     (lem:shrink-window
                      (- (lem:window-y (first *dragging-window*))
                         (second (cddr msg))))))
               (when (first *dragging-window*)
                 (setf *dragging-window*
                       (list nil (cddr msg) *dragging-window*))))))
      (when *message-on-mouse-event*
        (lem:message "mouse:~S" msg))
      (lem:redraw-display))))

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (format *terminal-io* "~A[?1000h~A[?1002h~A[?1006h~%" #\esc #\esc #\esc)
  (ignore-errors
   (dolist (window (lem:window-list))
     (lem::screen-clear (lem::window-screen window)))
   (lem:redraw-display))
  (run-hooks *enable-hook*))

(defun disable-hook ()
  (format *terminal-io* "~A[?1006l~A[?1002l~A[?1000l~%" #\esc #\esc #\esc)
  (ignore-errors
   (dolist (window (lem:window-list))
     (lem::screen-clear (lem::window-screen window)))
   (lem:redraw-display))
  (run-hooks *disable-hook*))

(define-minor-mode mouse-sgr-1006-mode
  (:global t
   :enable-hook #'enable-hook
   :disable-hook #'disable-hook))

(defun enable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode t))

(add-hook *after-init-hook* 'enable-mouse-sgr-1006-mode)

(eval-when (:load-toplevel)
  (enable-mouse-sgr-1006-mode))
