(defpackage :lem-winsize
  (:use :cl :cffi)
  (:export :win-row :win-col :win-size))

(in-package :lem-winsize)

(defcstruct winsize
  (ws-row :unsigned-short)
  (ws-col :unsigned-short)
  (ws-xpixel :unsigned-short)
  (ws-ypixel :unsigned-short))

(defcfun ioctl :int
  (fd :int)
  (cmd :int)
  &rest)

(defun win-size (fd)
  (with-foreign-object (ws '(:struct winsize))
    (when (= 0 (ioctl fd 21523 :pointer ws))
      (with-foreign-slots ((ws-row ws-col) ws (:struct winsize))
        (list ws-row ws-col)))))
