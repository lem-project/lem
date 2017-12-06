(defpackage :lem-vi-mode.ex
  (:use :cl
        :lem
        :lem-vi-mode.core
        :lem-vi-mode.ex-parser)
  (:export :vi-ex))
(in-package :lem-vi-mode.ex)

(defvar *ex-keymap* (make-keymap :name '*ex-keymap*))

(define-vi-state ex (:keymap *ex-keymap*) ())

(define-key *ex-keymap* "C-m" 'minibuffer-read-line-confirm)
(define-key *ex-keymap* "C-i" 'ex-complete)

(define-command ex-complete () ()
  )

(define-command vi-ex () ()
  (with-state 'ex
    (execute-ex (prompt-for-line ": " "" nil nil 'vi-ex))))

(defun execute-ex (string)
  (let ((lem-vi-mode.ex-command:*point* (current-point)))
    (multiple-value-bind (exp error) (parse-ex string)
      (when error (editor-error "syntax error"))
      (eval exp))))
