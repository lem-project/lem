;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/theme-policy
  (:use :cl)
  (:export :contrast-ratio
           :oklch
           :theme-violations
           :*min-text-contrast*
           :*max-syntax-chroma*
           :*syntax-roles*))
(in-package :yeditor-components/theme-policy)

;;;; Ergonomic policy for color themes (plan Cap. 12-A.4):
;;;; no pure black background, no pure white text, text/background contrast at
;;;; least 7:1 (WCAG AAA), and no neon in syntax colors (OKLCH chroma cap, plus a
;;;; cap on very light saturated colors, which chroma alone misses: #00ffff).

(defparameter *min-text-contrast* 7.0)
(defparameter *max-syntax-chroma* 0.17)
(defparameter *neon-lightness* 0.9)
(defparameter *neon-light-chroma* 0.12)
(defparameter *syntax-roles* (list :keyword :string :function :constant :type :variable :builtin))

(defun hex-rgb (hex)
  (loop :for i :in (list 1 3 5)
        :collect (parse-integer hex :start i :end (+ i 2) :radix 16)))

(defun linear (channel)
  (let ((c (/ channel 255d0)))
    (if (<= c 0.04045d0) (/ c 12.92d0) (expt (/ (+ c 0.055d0) 1.055d0) 2.4d0))))

(defun relative-luminance (hex)
  (destructuring-bind (r g b) (mapcar (function linear) (hex-rgb hex))
    (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b))))

(defun contrast-ratio (a b)
  "WCAG 2 contrast ratio between two #RRGGBB colors."
  (let ((la (relative-luminance a)) (lb (relative-luminance b)))
    (/ (+ (max la lb) 0.05d0) (+ (min la lb) 0.05d0))))

(defun oklch (hex)
  "Return (values L C) of a #RRGGBB color in OKLCH (Ottosson 2020)."
  (destructuring-bind (r g b) (mapcar (function linear) (hex-rgb hex))
    (let* ((l (expt (+ (* 0.4122214708d0 r) (* 0.5363325363d0 g) (* 0.0514459929d0 b)) 1/3))
           (m (expt (+ (* 0.2119034982d0 r) (* 0.6806995451d0 g) (* 0.1073969566d0 b)) 1/3))
           (s (expt (+ (* 0.0883024619d0 r) (* 0.2817188376d0 g) (* 0.6299787005d0 b)) 1/3))
           (lightness (+ (* 0.2104542553d0 l) (* 0.7936177850d0 m) (* -0.0040720468d0 s)))
           (a (+ (* 1.9779984951d0 l) (* -2.4285922050d0 m) (* 0.4505937099d0 s)))
           (bb (+ (* 0.0259040371d0 l) (* 0.7827717662d0 m) (* -0.8086757660d0 s))))
      (values lightness (sqrt (+ (* a a) (* bb bb)))))))

(defun theme-violations (roles)
  "Return a list of policy violation messages for a theme given as a plist of
   #RRGGBB colors with at least :bg :fg and the syntax roles. Empty list = passes."
  (let ((bg (getf roles :bg)) (fg (getf roles :fg)) (out nil))
    (when (string-equal bg "#000000") (push "background is pure black" out))
    (when (string-equal fg "#ffffff") (push "text is pure white" out))
    (let ((ratio (contrast-ratio fg bg)))
      (when (< ratio *min-text-contrast*)
        (push (format nil "text/background contrast ~,2F < ~,1F" ratio *min-text-contrast*) out)))
    (dolist (role *syntax-roles*)
      (let ((color (getf roles role)))
        (multiple-value-bind (l c) (oklch color)
          (cond ((> c *max-syntax-chroma*)
                 (push (format nil "~(~A~) ~A chroma ~,3F > ~,2F" role color c *max-syntax-chroma*) out))
                ((and (> l *neon-lightness*) (> c *neon-light-chroma*))
                 (push (format nil "~(~A~) ~A is neon (L ~,2F, C ~,3F)" role color l c) out))))))
    (nreverse out)))
