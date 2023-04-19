(defpackage :lem-sdl2/icon
  (:use :cl)
  (:export :init-application-icon))
(in-package :lem-sdl2/icon)

(defun init-application-icon (window)
  (let ((image (sdl2-image:load-image
                (asdf:system-relative-pathname :lem-sdl2 "resources/icon.png"))))
    (sdl2-ffi.functions:sdl-set-window-icon window image)
    (sdl2:free-surface image)))
