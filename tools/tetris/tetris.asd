(defpackage :lem-tetris-asd
  (:use :asdf))
(in-package :lem-tetris-asd)

(defsystem :lem-tetris
  :depends-on (:lem)
  :serial t
  :components ((:file "tetris")))
