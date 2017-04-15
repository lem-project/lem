(defpackage :lem-lisp-mode.errors
  (:use :cl)
  (:export :disconnected))

(in-package :lem-lisp-mode.errors)

(define-condition disconnected (simple-condition)
  ())
