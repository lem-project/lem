;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*info-mode-keymap*
          *info-mode-keymap*
          info-mode
          info-popup-closure
          info-popup))

(defvar *info-mode-keymap*
  (make-keymap "info"))

(define-minor-mode info-mode
  :name "info"
  :keymap *info-mode-keymap*)

(define-key *info-mode-keymap* (kbd "q") 'quit-window)

(defun info-popup (buffer &optional output-function (focus-set-p t) mode)
  (with-buffer-read-only buffer nil
    (buffer-erase buffer)
    (set-buffer-mode buffer 'info-mode t)
    (when mode
      (set-buffer-mode buffer mode))
    (when output-function
      (with-open-stream (out (make-buffer-output-stream buffer))
        (funcall output-function out))))
  (let ((window (display-buffer buffer)))
    (when focus-set-p
      (select-window window))
    window))
