(in-package :cl-user)
(defpackage :lem-slime-asd
  (:use :cl :asdf))
(in-package :lem-slime-asd)

(defsystem lem-slime
  :depends-on (:alexandria
               :trivial-types
               :usocket
               :swank
               :optima
               :uiop
               :lem)
  :serial t
  :components ((:file "errors")
               (:file "swank-protocol")
               (:file "slime")))
