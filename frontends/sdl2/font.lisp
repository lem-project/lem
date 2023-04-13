(defpackage :lem-sdl2/font
  (:use :cl)
  (:export :make-font
           :open-font))
(in-package :lem-sdl2/font)

(defstruct font
  size
  normal-file
  bold-file
  unicode-normal-file
  unicode-bold-file)

(defun open-font (font)
  (values (sdl2-ttf:open-font (font-normal-file font)
                              (font-size font))
          (sdl2-ttf:open-font (font-bold-file font)
                              (font-size font))
          (sdl2-ttf:open-font (font-unicode-normal-file font)
                              (font-size font))
          (sdl2-ttf:open-font (font-unicode-bold-file font)
                              (font-size font))))
