(defpackage :lem-sdl2/resource
  (:use :cl)
  (:export :get-resource-pathname))
(in-package :lem-sdl2/resource)

(defun get-resource-pathname (pathname)
  (or (lem:lem-relative-pathname pathname)
      (asdf:system-relative-pathname :lem-sdl2 pathname)))
