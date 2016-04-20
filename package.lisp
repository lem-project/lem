(in-package :cl-user)
(defpackage :lem.term
  (:use :cl)
  (:export
   :make-attribute
   :attribute
   :attribute-to-bits
   :with-allow-interrupt
   :term-init
   :term-finallize))

(defpackage :lem-internal
  (:use :cl))

(defpackage :lem
  (:use :cl :lem.fatstring :lem.util :lem-internal :lem.term))

(defpackage :lem-user
  (:use :cl :lem))
