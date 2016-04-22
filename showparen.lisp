;; -*- mode:lisp; packag:lem -*-

(in-package :lem)

(defvar *brackets-overlays* nil)

(add-hook 'post-command-hook
          (lambda ()
            (mapc #'delete-overlay *brackets-overlays*)
            (setq *brackets-overlays* nil)
            (let ((highlight-points))
              (when (syntax-open-paren-char-p (following-char))
                (save-excursion
                 (when (forward-sexp 1 t)
                   (push (progn (shift-position -1) (current-point))
                         highlight-points))))
              (when (syntax-closed-paren-char-p (preceding-char))
                (save-excursion
                 (when (backward-sexp 1 t)
                   (push (current-point) highlight-points))))
              (let ((attr (make-attribute "cyan" :reverse-p t)))
                (dolist (point highlight-points)
                  (push (make-overlay point
                                      (make-point (point-linum point)
                                                  (1+ (point-charpos point)))
                                      attr)
                        *brackets-overlays*))
                (if highlight-points t nil)))))
