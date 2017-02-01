(in-package :cl-user)

(defpackage :lem-interface
  (:use :cl :lem.term :lem.util)
  (:export :display-init
           :display-finalize
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
           :redraw-display
           :update-display-size
           :print-echoarea
           ))

(defpackage :lem
  (:use :cl :lem.util :lem-interface :lem-base)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym))))
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-interface)
                     :collect (make-symbol (string sym)))))

(defpackage :lem-user
  (:use :cl :lem))

(use-package :lem :lem-interface)
