;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*prog-mode-keymap*
          prog-mode
          prog-indent-line))

(defvar *prog-mode-keymap* (make-keymap))

(define-major-mode prog-mode nil
  (:name "prog"
   :keymap *prog-mode-keymap*)
  (buffer-put (window-buffer)
              :indent-tabs-mode t))

(define-key *prog-mode-keymap* (kbd "C-i") 'prog-indent-line)
(define-command prog-indent-line () ()
  (let* ((f (buffer-get (window-buffer) :calc-indent-function))
         (n (and f (funcall f))))
    (if n
        (indent-line n)
        (insert-char #\tab 1))))

(define-key *prog-mode-keymap* (kbd "C-j") 'prog-newline-and-indent)
(define-key *prog-mode-keymap* (kbd "M-j") 'prog-newline-and-indent)
(define-command prog-newline-and-indent (n) ("p")
  (insert-newline n)
  (prog-indent-line))
