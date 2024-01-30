(defpackage #:lem-template/render
  (:use :cl :lem)
  (:export #:render-file))
(in-package :lem-template/render)

(defun render-file (template-file &optional args)
  "Render a cl-template file to a string."
  (funcall
   (cl-template:compile-template
    (uiop:read-file-string template-file))
   args))
