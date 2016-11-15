(in-package :cl-user)

(defpackage :lem-interface
  (:use :cl :lem.fatstring :lem.term :lem.util)
  (:export :display-init
           :make-screen
           :screen-delete
           :screen-modify
           :screen-set-size
           :screen-set-pos
           :screen-clear
           :redraw-display-window
           :display-width
           :display-height
           :screen-erase
           :screen-print-string
           :screen-move-cursor
           :print-echoarea
           :get-char
           :redraw-display))

(defpackage :lem
  (:use :cl :lem.util :lem.term :lem-interface))

(defpackage :lem-user
  (:use :cl :lem))

(use-package :lem :lem-interface)

(export '#.(loop :for sym :being :the :external-symbols :of (find-package :lem-interface)
                 :collect sym)
        :lem)
