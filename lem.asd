(defpackage :lem
  (:use :cl)
  (:export :lem))

(defsystem lem
  :serial t
  :components ()
  :depends-on ("cl-ncurses"))
