(defpackage :lem.line-numbers
  (:use :cl :lem)
  (:export :line-numbers-attribute
           :line-numbers)
  #+sbcl
  (:lock t))
(in-package :lem.line-numbers)

(defvar *initialized* nil)
(defvar *line-number-format* nil)

(define-attribute line-numbers-attribute
  (:light :foreground "blue" :background "#e0e0e0")
  (:dark :foreground "snow" :background "gray20"))

(define-editor-variable line-numbers nil ""
  (lambda (value)
    (if value
        (line-numbers-on)
        (line-numbers-off))))

(define-condition update (after-executing-command) ())
(defmethod handle-signal ((condition update))
  (update))

(defun update (&optional (window (current-window)))
  (let ((buffer (window-buffer window)))
    (mapc #'delete-overlay (buffer-value buffer 'line-number-overlays))
    (when (variable-value 'line-numbers :default buffer)
      (let ((overlays '())
            (current-line-number (line-number-at-point (lem::window-point (current-window)))))
        (dolist (window (get-buffer-windows buffer))
          (with-point ((p (window-view-point window)))
            (let ((n (length (prin1-to-string (buffer-nlines buffer)))))
              (loop :for linum :from (line-number-at-point p) :repeat (window-height window)
                    :do (let ((ov (make-overlay p p 'line-numbers-attribute)))
                          (overlay-put ov :display-left t)
                          (if *line-number-format*
                              (overlay-put ov :text
                                           (if (eql linum current-line-number)
                                               (format nil "~vD " n linum)
                                               (format nil "~v@D " n
                                                       (- linum current-line-number))))
                              (overlay-put ov :text (format nil "~vD " n linum)))
                          (push ov overlays))
                        (unless (line-offset p 1)
                          (return))))))
        (setf (buffer-value buffer 'line-number-overlays) overlays)))))

(defun line-numbers-init ()
  (unless *initialized*
    (setf *initialized* t)
    (add-hook *window-scroll-functions* 'update)))

(defun line-numbers-on ()
  (unless (variable-value 'line-numbers :global)
    (line-numbers-init)))

(defun line-numbers-off ()
  (when (variable-value 'line-numbers)
    (setf *initialized* nil)
    (remove-hook *window-scroll-functions* 'update)
    (dolist (buffer (buffer-list))
      (mapc #'delete-overlay (buffer-value buffer 'line-number-overlays)))))

(define-command toggle-line-numbers () ()
  (setf (variable-value 'line-numbers :global)
        (not (variable-value 'line-numbers :global))))

(add-hook *after-init-hook*
          (lambda ()
            (when (variable-value 'line-numbers :global)
              (line-numbers-init))))
