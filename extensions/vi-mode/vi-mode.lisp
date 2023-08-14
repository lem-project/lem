(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem-vi-mode/core
        :lem-vi-mode/ex)
  (:import-from :lem-vi-mode/options
                :vi-option-value)
  (:export :vi-mode
           :define-vi-state
           :*command-keymap*
           :*insert-keymap*
           :*ex-keymap*
           :vi-option-value))
