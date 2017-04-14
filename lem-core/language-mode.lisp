(defpackage :lem.language-mode
  (:use :cl :lem)
  (:export
   :*language-mode-keymap*
   :language-mode
   :indent
   :newline-and-indent
   :indent-region))
(in-package :lem.language-mode)

(define-major-mode language-mode ()
    (:keymap *language-mode-keymap*)
  nil)

(define-key *language-mode-keymap* (kbd "C-i") 'indent)
(define-key *language-mode-keymap* (kbd "C-j") 'newline-and-indent)
(define-key *language-mode-keymap* (kbd "M-j") 'newline-and-indent)
(define-key *language-mode-keymap* (kbd "C-M-\\") 'indent-region)

(define-command indent (&optional (n 1)) ("p")
  (if (variable-value 'calc-indent-function)
      (indent-line (current-point))
      (self-insert n)))

(define-command newline-and-indent (n) ("p")
  (newline n)
  (indent))

(define-command indent-region (start end) ("r")
  (save-excursion
    (apply-region-lines start end 'indent-line)))
