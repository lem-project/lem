(defpackage :lem-lisp-mode/connections
  (:use :cl)
  (:export :connection-list))
(in-package :lem-lisp-mode/connections)

(defvar *connection-list* '())

(defun connection-list ()
  *connection-list*)

(defun (setf connection-list) (list)
  (setf *connection-list* list))
