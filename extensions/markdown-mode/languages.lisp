(defpackage :lem-markdown-mode/languages
  (:use :cl)
  (:export :find-mode-by-language-name))
(in-package :lem-markdown-mode/languages)

(defparameter *language-mode-pairs*
  `(("common-lisp" . lem-lisp-mode:lisp-mode)
    ("lisp" . lem-lisp-mode:lisp-mode)
    ("shell" . lem-posix-shell-mode:posix-shell-mode)))

(defun find-mode-by-language-name (language-name)
  (or (cdr (assoc language-name *language-mode-pairs* :test #'equal))
      (lem:find-mode language-name)))
