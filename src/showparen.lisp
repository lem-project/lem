(in-package :cl-user)
(defpackage :lem.show-paren
  (:use :cl :lem))
(in-package :lem.show-paren)

;; (defvar *brackets-overlays* nil)

;; (defvar *paren-attribute* (make-attribute "cyan" nil :reverse-p t))

;; (defun show-paren-timer-function ()
;;   (mapc #'delete-overlay *brackets-overlays*)
;;   (setq *brackets-overlays* nil)
;;   (let ((highlight-points '()))
;;     (when (syntax-open-paren-char-p (following-char))
;;       (let ((goal-point (form-offset (copy-point (current-point) :temporary) 1)))
;;         (when goal-point
;;           (push (character-offset goal-point -1)
;;                 highlight-points))))
;;     (when (syntax-closed-paren-char-p (preceding-char))
;;       (let ((goal-point (form-offset (copy-point (current-point) :temporary) -1)))
;;         (when goal-point
;;           (push goal-point highlight-points))))
;;     (dolist (point highlight-points)
;;       (push (make-overlay point
;;                           (character-offset (copy-point point :temporary) 1)
;;                           *paren-attribute*)
;;             *brackets-overlays*))
;;     (when highlight-points
;;       (redraw-display))))

;; (start-idle-timer "show-paren" 100 t
;;                   'show-paren-timer-function)
