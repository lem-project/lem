(defpackage :lem-utils/random
  (:use :cl)
  (:export :random-range))
(in-package :lem-utils/random)

#+sbcl
(sb-ext:lock-package :lem-utils/random)

(defun random-range (min max &optional (state *random-state*))
  (+ min (random (- max min) state)))
