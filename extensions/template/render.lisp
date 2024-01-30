(defpackage #:lem-template/render
  (:use :cl :lem)
  (:export #:render-file #:render-string))
(in-package :lem-template/render)

(defun render-string (string &optional args)
  "Render a cl-template string to a string."
  (funcall (cl-template:compile-template string) args))

(defun render-file (template-file &optional args)
  "Render a cl-template file to a string."
  (funcall
   (cl-template:compile-template
    (uiop:read-file-string template-file))
   args))
