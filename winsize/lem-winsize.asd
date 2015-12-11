#-asdf
(require :asdf)

(defpackage :lem-winsize-asd
  (:use :cl :asdf))

(in-package :lem-winsize-asd)

(defsystem lem-winsize
  :serial t
  :components ((:file "winsize"))
  :depends-on (:cffi))
