(defpackage :lem.show-paren
  (:use :cl :lem)
  (:export :showparen-attribute
           :forward-matching-paren
           :backward-matching-paren))
(in-package :lem.show-paren)

(defvar *brackets-overlays* nil)


(define-attribute showparen-attribute
  (t :background "cyan"))

(define-editor-variable forward-matching-paren 'forward-matching-paren-default)
(define-editor-variable backward-matching-paren 'backward-matching-paren-default)

(defun forward-matching-paren-default (p)
  (when (syntax-open-paren-char-p (character-at p))
    (let ((goal-point (scan-lists (copy-point p :temporary) 1 0 t)))
      (when goal-point
        (character-offset goal-point -1)))))

(defun backward-matching-paren-default (p)
  (when (syntax-closed-paren-char-p (character-at p -1))
    (scan-lists (copy-point p :temporary) -1 0 t)))

(defun show-paren-timer-function ()
  (mapc #'delete-overlay *brackets-overlays*)
  (setq *brackets-overlays* nil)
  (let ((highlight-points '()))
    (alexandria:when-let ((p (funcall (variable-value 'forward-matching-paren)  (current-point))))
      (push (copy-point p :temporary) highlight-points))
    (alexandria:when-let ((p (funcall (variable-value 'backward-matching-paren) (current-point))))
      (push (copy-point p :temporary) highlight-points))
    (dolist (point highlight-points)
      (push (make-overlay point
                          (character-offset (copy-point point :temporary) 1)
                          'showparen-attribute)
            *brackets-overlays*))))

(defvar *show-paren-timer*)

(when (or (not (boundp '*show-paren-timer*))
          (not (timer-alive-p *show-paren-timer*)))
  (setf *show-paren-timer*
        (start-idle-timer 100 t
                          'show-paren-timer-function nil "show paren timer")))
