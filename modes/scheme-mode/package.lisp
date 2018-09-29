(defpackage :lem-scheme-mode
  (:use :cl
        :lem
        :lem.language-mode)
  (:export
   ;; scheme-mode.lisp
   :*scheme-mode-keymap*
   :*scheme-mode-hook*
   :scheme-beginning-of-defun
   :scheme-end-of-defun
   :scheme-indent-sexp
   :scheme-eval-last-expression
   :scheme-eval-region
   :*scheme-run-command*))
