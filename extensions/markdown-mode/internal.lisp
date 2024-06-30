(defpackage :lem-markdown-mode/internal
  (:use :cl)
  (:export :on-save
           :on-kill))
(in-package :lem-markdown-mode/internal)

(defgeneric on-save (buffer))
(defgeneric on-kill (buffer))
