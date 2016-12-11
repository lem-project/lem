(in-package :cl-user)
(defpackage :lem-shell-asd
  (:use :cl :asdf))
(in-package :lem-shell-asd)

(defsystem lem-shell
  :depends-on (:lem
               :uiop)
  :serial t
  :components ((:file "shell")))
