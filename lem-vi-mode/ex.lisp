(in-package :lem-vi-mode)

(define-vi-state ex (:keymap *global-keymap*))

(define-command ex-complete () ()
  )

(define-command vi-ex () ()
  (with-state 'ex
    (execute-ex (prompt-for-line ": " "" nil nil 'vi-ex))))

(defun execute-ex (string)
  (declare (ignore string)))
