#-asdf
(require :asdf)

(defpackage :leval-server-asd
  (:use :cl :asdf))

(in-package :leval-server-asd)

(defsystem leval-server
  :serial t
  :components ((:file "leval-server"))
  :depends-on (:cl-readline
               :usocket
               :bordeaux-threads
               :cl-ppcre))
