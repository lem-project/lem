(defpackage :lem-scheme-mode
  (:use :cl
        :lem
        :lem.completion-mode
        :lem.language-mode)
  (:export
   ;; scheme-mode.lisp
   :*scheme-mode-keymap*
   :*scheme-mode-hook*
   :*scheme-run-command*
   :*scheme-completion-names*
   :scheme-beginning-of-defun
   :scheme-end-of-defun
   :scheme-indent-sexp
   ;; eval.lisp
   :scheme-eval-last-expression
   :scheme-eval-region))
