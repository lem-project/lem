(uiop:define-package :lem
  (:use :cl)
  (:use-reexport :lem-core))
(sb-ext:lock-package :lem)

(defpackage :lem-user
  (:use :cl :lem))
