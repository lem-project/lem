(in-package :cl-user)

(defpackage :lem-interface
  (:use :cl :lem.fatstring :lem.term :lem.util)
  (:export :display-init
           :display-finalize
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
           :redraw-display
           :call-with-allow-interrupt))

;; (defpackage :lem-interface.ncurses
;;   (:use :cl :lem.fatstring :lem.term :lem.util)
;;   (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-interface)
;;                      :collect sym)))

;; (defpackage :lem-interface.null
;;   (:use :cl)
;;   (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-interface)
;;                      :collect sym)))

(defpackage :lem
  (:use :cl :lem.util :lem-interface)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-interface)
                     :collect (make-symbol (string sym)))))

(defpackage :lem-user
  (:use :cl :lem))

(use-package :lem :lem-interface)
