(defpackage :lem-vi-mode.mode
  (:use :cl :lem)
  (:export :*enable-hook*
           :*disable-hook*
           :vi-mode))
(in-package :lem-vi-mode.mode)

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (run-hooks *enable-hook*))

(defun disable-hook ()
  (run-hooks *disable-hook*))

(define-minor-mode vi-mode
    (:global t
     :enable-hook #'enable-hook
     :disable-hook #'disable-hook))
