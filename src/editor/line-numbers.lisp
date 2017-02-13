(defpackage :lem.line-numbers
  (:use :cl :lem))
(in-package :lem.line-numbers)

(defvar *visited* nil)
(defvar *attribute* (make-attribute "blue" nil))

(defun update (&optional (window (current-window)))
  (let ((buffer (window-buffer window)))
    (mapc #'delete-overlay (buffer-value buffer 'line-number-overlays))
    (when (buffer-value buffer 'line-numbers)
      (let ((overlays '()))
        (with-point ((p (window-view-point window)))
          (let ((n (length (prin1-to-string (buffer-nlines buffer)))))
            (loop :for linum :from (line-number-at-point p) :repeat (window-height window)
                  :do
                  (let ((ov (make-overlay p p *attribute*)))
                    (overlay-put ov :display-left t)
                    (overlay-put ov :text (format nil "~vD " n linum))
                    (push ov overlays))
                  (unless (line-offset p 1)
                    (return)))))
        (setf (buffer-value buffer 'line-number-overlays) overlays)))))

(define-command enable-line-numbers () ()
  (setf (buffer-value (current-buffer) 'line-numbers) t)
  (unless *visited*
    (setf *visited* t)
    (add-hook *post-command-hook* 'update)))

;; (define-command disable-line-numbers () ()
;;   )
