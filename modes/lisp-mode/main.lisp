(in-package :lem-lisp-mode)

(add-hook *before-init-hook*
          (lambda ()
            (start-lisp-repl t)))
