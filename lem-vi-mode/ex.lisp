(in-package :lem-vi-mode)

(defvar *ex-keymap* (make-keymap :name '*ex-keymap*))

(define-vi-state ex (:keymap *ex-keymap*))

(define-key *ex-keymap* "C-m" 'minibuffer-read-line-confirm)
(define-key *ex-keymap* "C-i" 'ex-complete)

(define-command ex-complete () ()
  )

(define-command vi-ex () ()
  (with-state 'ex
    (execute-ex (prompt-for-line ": " "" nil nil 'vi-ex))))

(defun execute-ex (string)
  (declare (ignore string)))
