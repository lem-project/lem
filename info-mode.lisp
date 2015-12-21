;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*info-mode-keymap*
          info-mode
          info-popup))

(defvar *info-mode-keymap*
  (make-keymap "info"))

(define-minor-mode info-mode
  :name "info"
  :keymap *info-mode-keymap*)

(define-key *info-mode-keymap* (kbd "q") 'quit-window)

(defun info-popup (buffer &optional output-function (focus-set-p t) mode)
  (let ((window (display-buffer buffer)))
    (with-current-window window
      (with-buffer-read-only buffer nil
        (when output-function
          (erase-buffer))
        (info-mode t)
        (when mode (funcall mode))
        (when output-function
          (save-excursion
           (with-open-stream (out (make-buffer-output-stream buffer))
             (funcall output-function out))))))
    (when focus-set-p
      (select-window window))
    window))
