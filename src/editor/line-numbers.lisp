(defpackage :lem.line-numbers
  (:use :cl :lem))
(in-package :lem.line-numbers)

(defvar *visited* nil)
(defvar *toggle* nil)
(defvar *attribute* (make-attribute "blue" nil))

(defun update (&optional (window (current-window)))
  (let ((buffer (window-buffer window)))
    (when (buffer-value buffer 'line-numbers)
      (with-point ((p (window-view-point window)))
        (let ((n (length (prin1-to-string (buffer-nlines buffer)))))
          (loop :for linum :from (line-number-at-point p) :repeat (window-height window)
                :do
                (lem-base::set-left-fringe
                 p (format nil "~vD " n linum) *attribute*)
                (unless (line-offset p 1)
                  (return))))))))

(define-command enable-line-numbers () ()
  (setf (buffer-value (current-buffer) 'line-numbers) t)
  (unless *visited*
    (setf *visited* t)
    (add-hook *post-command-hook* 'update)))

;; (define-command disable-line-numbers () ()
;;   )
