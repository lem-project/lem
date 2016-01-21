;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*prog-mode-keymap*
          prog-mode
          prog-indent-line
          prog-newline-and-indent
          prog-indent-region))

(defvar *prog-mode-keymap* (make-keymap))

(define-major-mode prog-mode nil
  (:name "prog"
   :keymap *prog-mode-keymap*)
  (setf (get-bvar :indent-tabs-mode) t))

(define-key *prog-mode-keymap* (kbd "C-i") 'prog-indent-line)
(define-command prog-indent-line () ()
  (let* ((f (get-bvar :calc-indent-function))
         (n (and f (funcall f))))
    (if n
        (indent-line n)
        (insert-char #\tab 1))))

(define-key *prog-mode-keymap* (kbd "C-j") 'prog-newline-and-indent)
(define-key *prog-mode-keymap* (kbd "M-j") 'prog-newline-and-indent)
(define-command prog-newline-and-indent (n) ("p")
  (insert-newline n)
  (prog-indent-line))

(define-key *prog-mode-keymap* (kbd "C-M-\\") 'prog-indent-region)
(define-command prog-indent-region () ()
  (save-excursion
   (apply-region-lines (region-beginning)
                       (region-end)
                       'prog-indent-line)))
