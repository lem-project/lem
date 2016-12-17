(in-package :cl-user)
(defpackage :lem.show-paren
  (:use :cl :lem))
(in-package :lem.show-paren)

(defvar *brackets-overlays* nil)

(defvar *paren-attribute* (make-attribute "cyan" nil :reverse-p t))

(defun show-paren-timer-function ()
  (mapc #'delete-overlay *brackets-overlays*)
  (setq *brackets-overlays* nil)
  (let ((highlight-markers '()))
    (when (syntax-open-paren-char-p (following-char))
      (let ((goal-marker (lem::form-offset (copy-marker (current-marker) :temporary) 1)))
        (when goal-marker
          (push (lem::character-offset goal-marker -1)
                highlight-markers))))
    (when (syntax-closed-paren-char-p (preceding-char))
      (let ((goal-marker (lem::form-offset (copy-marker (current-marker) :temporary) -1)))
        (when goal-marker
          (push goal-marker highlight-markers))))
    (dolist (marker highlight-markers)
      (push (make-overlay marker
                          (lem::character-offset (copy-marker marker :temporary) 1)
                          *paren-attribute*)
            *brackets-overlays*))
    (when highlight-markers
      (redraw-display))))

(start-idle-timer "show-paren" 100 t
                  'show-paren-timer-function)
