(defpackage :lem-vi-mode.ex-util
  (:use :cl)
  (:export :syntax-error))
(in-package :lem-vi-mode.ex-util)

(defun syntax-error ()
  (lem:editor-error "syntax error"))
