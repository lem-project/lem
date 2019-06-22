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
   :*scheme-load-command*
   :*scheme-completion-names*
   :scheme-keyword-data
   :scheme-beginning-of-defun
   :scheme-end-of-defun
   :scheme-indent-sexp
   :scheme-load-file
   ;; eval.lisp
   :scheme-kill-process
   :scheme-eval-last-expression
   :scheme-eval-region
   ;; repl.lisp
   :scheme-repl-mode
   :*scheme-repl-mode-keymap*
   :start-scheme-repl
   :scheme-switch-to-repl-buffer
   :scheme-eval-or-newline))
