(in-package :lem-vi-mode)

(define-vi-state ex (:keymap *global-keymap*))

(define-command ex-complete () ()
  )

(define-command vi-ex () ()
  (prompt-for-line ": " "" nil nil 'vi-ex))
