(defpackage :lem-core/commands/frame
  (:use :cl :lem-core)
  (:export :toggle-frame-fullscreen)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/frame)

(define-command toggle-frame-fullscreen () ()
  "Toggles fullscreen."
  (setf (display-fullscreen-p) (not (display-fullscreen-p))))
