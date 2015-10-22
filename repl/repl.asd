#-asdf
(require :asdf)

(defpackage :repl-asd
  (:use :cl :asdf))

(in-package :repl-asd)

(defsystem repl
  :serial t
  :components ((:file "package")
               (:file "complete")
               (:file "leval-server")
               (:file "repl"))
  :depends-on (:cl-readline
               :usocket
               :bordeaux-threads
               :cl-ppcre))
