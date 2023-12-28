(defpackage :lem-jsonrpc/view
  (:use :cl)
  (:export :view
           :make-view
           :view-window
           :view-id
           :view-x
           :view-y
           :view-width
           :view-height
           :view-use-modeline
           :view-kind))
(in-package :lem-jsonrpc/view)

(defvar *view-id-counter* 0)

(defstruct (view (:constructor %make-view))
  (id (incf *view-id-counter*))
  window
  x
  y
  width
  height
  use-modeline
  kind)

(defun make-view (&rest args &key window x y width height use-modeline kind)
  (declare (ignore window x y width height use-modeline kind))
  (apply #'%make-view args))
