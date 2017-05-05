(in-package :cl-user)

(defpackage :lem-interface
  (:use :cl)
  (:export :define-interface
           :define-implementation
           :set-foreground
           :set-background
           :display-background-mode
           :call-with-screen
           :make-screen
           :screen-delete
           :screen-modify
           :screen-set-size
           :screen-set-pos
           :screen-clear
           :display-width
           :display-height
           :screen-erase
           :screen-print-string
           :screen-move-cursor
           :redraw-display-window
           :update-display
           :update-display-size
           :input-loop))

(defpackage :lem
  (:use :cl :lem-base :lem-interface)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym))))
  (:export :set-foreground
           :set-background))

(defpackage :lem-user
  (:use :cl :lem))
