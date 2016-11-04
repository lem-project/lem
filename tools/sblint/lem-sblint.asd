(defpackage :lem-sblint-asd
  (:use :cl :asdf))
(in-package :lem-sblint-asd)

(defsystem lem-sblint
  :depends-on (:lem :sblint :cl-ppcre)
  :serial t
  :components ((:file "lem-sblint")))
