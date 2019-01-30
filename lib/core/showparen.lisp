(defpackage :lem.show-paren
  (:use :cl :lem)
  (:export :showparen-attribute
           :forward-matching-paren
           :backward-matching-paren))
(in-package :lem.show-paren)

(defvar *brackets-overlays* '())

(define-attribute showparen-attribute
  (t :background "cyan"))

(define-editor-variable forward-matching-paren 'forward-matching-paren-default)
(define-editor-variable backward-matching-paren 'backward-matching-paren-default)

(defun forward-matching-paren-default (p)
  (when (syntax-open-paren-char-p (character-at p))
    (with-point ((limit (window-view-point (current-window))))
      (unless (line-offset limit (window-height (current-window)))
        (buffer-end limit))
      (let ((goal-point (scan-lists (copy-point p :temporary) 1 0 t limit)))
        (when goal-point
          (character-offset goal-point -1))))))

(defun backward-matching-paren-default (p)
  (when (syntax-closed-paren-char-p (character-at p -1))
    (scan-lists (copy-point p :temporary) -1 0 t (window-view-point (current-window)))))

(defun show-paren-function ()
  (mapc #'delete-overlay *brackets-overlays*)
  (setq *brackets-overlays* nil)
  (let ((highlight-points '()))
    (or (alexandria:when-let ((p (funcall (variable-value 'backward-matching-paren) (current-point))))
          (push (copy-point p :temporary) highlight-points)
          (alexandria:when-let ((p (funcall (variable-value 'forward-matching-paren) p)))
            (push (copy-point p :temporary) highlight-points)))
        (alexandria:when-let ((p (funcall (variable-value 'forward-matching-paren)  (current-point))))
          (push (copy-point p :temporary) highlight-points)))
    (dolist (point highlight-points)
      (push (make-overlay point
                          (character-offset (copy-point point :temporary) 1)
                          'showparen-attribute)
            *brackets-overlays*))))

(defvar *show-paren-timer* nil)

(define-command toggle-show-paren () ()
  (let ((p (not *show-paren-timer*)))
    (when (interactive-p)
      (message "show paren ~:[dis~;en~]abled." p))
    (if p
        (progn
          (when *show-paren-timer*
            (stop-timer *show-paren-timer*))
          (setf *show-paren-timer*
                (start-idle-timer 100 t
                                  'show-paren-function nil "show paren timer"))
          t)
        (progn
          (when *show-paren-timer*
            (stop-timer *show-paren-timer*))
          (mapc #'delete-overlay *brackets-overlays*)
          (setq *brackets-overlays* nil)
          (setf *show-paren-timer* nil)))))

(unless *show-paren-timer*
  (toggle-show-paren))
