(defpackage :lem.line-numbers
  (:use :cl :lem)
  (:export :line-numbers-attribute))
(in-package :lem.line-numbers)

(defvar *visited* nil)

(define-attribute line-numbers-attribute
  (t :foreground "dark blue"))

(defun update (&optional (window (current-window)))
  (let ((buffer (window-buffer window)))
    (mapc #'delete-overlay (buffer-value buffer 'line-number-overlays))
    (when (buffer-value buffer 'line-numbers)
      (let ((overlays '()))
        (dolist (window (get-buffer-windows buffer))
          (with-point ((p (window-view-point window)))
            (let ((n (length (prin1-to-string (buffer-nlines buffer)))))
              (loop :for linum :from (line-number-at-point p) :repeat (window-height window)
                    :do
                    (let ((ov (make-overlay p p 'line-numbers-attribute)))
                      (overlay-put ov :display-left t)
                      (overlay-put ov :text (format nil "~vD " n linum))
                      (push ov overlays))
                    (unless (line-offset p 1)
                      (return))))))
        (setf (buffer-value buffer 'line-number-overlays) overlays)))))

(define-command enable-line-numbers () ()
  (setf (buffer-value (current-buffer) 'line-numbers) t)
  (unless *visited*
    (setf *visited* t)
    (add-hook *post-command-hook* 'update)
    (add-hook *window-scroll-functions* 'update)))

(define-command disable-line-numbers () ()
  (let ((buffer (current-buffer)))
    (when (buffer-value buffer 'line-numbers)
      (mapc #'delete-overlay (buffer-value buffer 'line-number-overlays))
      (setf (buffer-value buffer 'line-numbers) nil))))

(define-command toggle-line-numbers () ()
  (if (buffer-value (current-buffer) 'line-numbers)
      (disable-line-numbers)
      (enable-line-numbers)))
