(in-package :cl-user)
(defpackage :lem.show-paren
  (:use :cl :lem)
  (:export :showparen-attribute))
(in-package :lem.show-paren)

(defvar *brackets-overlays* nil)

(define-attribute showparen-attribute
  (t :background "cyan"))

(defun show-paren-timer-function ()
  (let ((shew-p (if *brackets-overlays* t nil)))
    (mapc #'delete-overlay *brackets-overlays*)
    (setq *brackets-overlays* nil)
    (let ((highlight-points '()))
      (when (syntax-open-paren-char-p (character-at (current-point)))
        (let ((goal-point (form-offset (copy-point (current-point) :temporary) 1)))
          (when goal-point
            (push (character-offset goal-point -1)
                  highlight-points))))
      (when (syntax-closed-paren-char-p (character-at (current-point) -1))
        (let ((goal-point (form-offset (copy-point (current-point) :temporary) -1)))
          (when goal-point
            (push goal-point highlight-points))))
      (dolist (point highlight-points)
        (push (make-overlay point
                            (character-offset (copy-point point :temporary) 1)
                            'showparen-attribute)
              *brackets-overlays*))
      (when (or shew-p highlight-points)
        (redraw-display)))))

(defvar *show-paren-timer*)

(when (or (not (boundp '*show-paren-timer*))
          (not (timer-alive-p *show-paren-timer*)))
  (setf *show-paren-timer*
        (start-idle-timer "show-paren" 100 t
                          'show-paren-timer-function)))
