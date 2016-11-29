(in-package :cl-user)
(defpackage :lem-slime-asd
  (:use :cl :asdf))
(in-package :lem-slime-asd)

(defsystem lem-slime
  :depends-on (;:lem
               :alexandria
               :trivial-types
               :usocket
               :swank
               :optima
               :uiop)
  :serial t
  :components ((:file "clhs")
               (:file "swank-protocol")
               (:file "lime")
               (:file "slime")
               ))
