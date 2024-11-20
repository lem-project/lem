(defpackage :lem-sdl2/frame
  (:use :cl :lem)
  (:export :maximize-frame
           :minimize-frame))
(in-package :lem-sdl2/frame)

(define-command maximize-frame () ()
  "Maximize the frame."
  (sdl2:in-main-thread ()
    (sdl2:maximize-window (lem-sdl2/display::display-window (lem-sdl2/display:current-display)))))

(define-command minimize-frame () ()
  "Minimize the frame."
  (sdl2:in-main-thread ()
    (sdl2:minimize-window (lem-sdl2/display::display-window (lem-sdl2/display:current-display)))))
  