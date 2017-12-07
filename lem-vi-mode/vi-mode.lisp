(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem-vi-mode.core
        :lem-vi-mode.ex
        )
  (:export :vi-mode
           :define-vi-state
           :*command-keymap*
           :*insert-keymap*
           :*ex-keymap*))
