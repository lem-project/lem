(defpackage :lem-sdl2/platform
  (:use :cl)
  (:export :platform
           :linux
           :mac
           :windows
           :get-platform))
(in-package :lem-sdl2/platform)

(defclass platform () ())
(defclass linux (platform) ())
(defclass windows (platform) ())
(defclass mac (platform) ())

(defvar *platform* nil)

(defun get-platform ()
  (or *platform*
      (setf *platform*
            (let ((platform-name (sdl2:platform)))
              (alexandria:switch (platform-name :test #'equal)
                ("Linux"
                 (make-instance 'linux))
                ("Mac OS X"
                 (make-instance 'mac))
                ("Windows"
                 (make-instance 'windows))
                (otherwise
                 (error "unsupported platform: ~A" (sdl2:platform))))))))
