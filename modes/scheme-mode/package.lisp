(defpackage :lem-scheme-mode
  (:use :cl
        :lem
        :lem.language-mode)
  (:export
   ;; scheme-mode.lisp
   :*scheme-mode-keymap*
   :scheme-beginning-of-defun
   :scheme-end-of-defun
   :insert-\(\)
   :move-over-\)
   :scheme-indent-sexp))
