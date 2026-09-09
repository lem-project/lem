(defpackage :lem-sdl2/sdl2
  (:use :cl)
  (:export :sdl2))
(in-package :lem-sdl2/sdl2)

(defclass sdl2 (lem:implementation)
  ()
  (:default-initargs
   :name :sdl2
   :redraw-after-modifying-floating-window nil
   :underline-color-support t))

(pushnew :lem-sdl2 *features*)
