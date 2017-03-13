(defpackage :lem-slime.errors
  (:use :cl)
  (:export :disconnected))

(in-package :lem-slime.errors)

(define-condition disconnected (simple-condition)
  ())
