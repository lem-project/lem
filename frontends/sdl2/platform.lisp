(defpackage :lem-sdl2/platform
  (:use :cl)
  (:export :linux
           :mac
           :get-platform))
(in-package :lem-sdl2/platform)

(defclass platform () ())
(defclass linux (platform) ())
(defclass mac (platform) ())

(defvar *platform* nil)

(defun get-platform ()
  (or *platform*
      (setf *platform*
            (cond ((equal "Linux" (sdl2:platform))
                   (make-instance 'linux))
                  ((equal "Mac OS X" (sdl2:platform))
                   (make-instance 'mac))
                  (t
                   (error "unsupported platform: ~A" (sdl2:platform)))))))
