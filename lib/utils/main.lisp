(defpackage :lem-utils/main
  (:nicknames :lem-utils)
  (:use :cl)
  (:export :utf8-bytes
           :bests-if
           :max-if
           :min-if
           :find-tree
           :random-range))
(in-package :lem-utils/main)

#+sbcl
(sb-ext:lock-package :lem-utils/main)

(defun utf8-bytes (c)
  (cond
    ((<= c #x7f) 1)
    ((<= #xc2 c #xdf) 2)
    ((<= #xe0 c #xef) 3)
    ((<= #xf0 c #xf4) 4)
    (t 1)))

(defun bests-if (fn list test)
  (let ((best-value)
        (bests))
    (dolist (x list)
      (let ((score (funcall fn x)))
        (cond ((or (not best-value)
                   (funcall test score best-value))
               (setq best-value score)
               (setq bests (list x)))
              ((= best-value score)
               (push x bests)))))
    (values bests best-value)))

(defun max-if (fn list)
  (bests-if fn list #'>))

(defun min-if (fn list)
  (bests-if fn list #'<))

(defun find-tree (x tree)
  (cond ((null tree) nil)
        ((eql x tree) x)
        ((consp tree)
         (or (find-tree x (car tree))
             (find-tree x (cdr tree))))))

(defun random-range (min max &optional (state *random-state*))
  (+ min (random (- max min) state)))
