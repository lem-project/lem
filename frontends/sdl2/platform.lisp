(defpackage :lem-sdl2/platform
  (:use :cl)
  (:export :platform
           :linux
           :mac
           :freebsd
           :windows
           :get-platform))
(in-package :lem-sdl2/platform)

(defclass platform () ())
(defclass linux (platform) ())
(defclass windows (platform) ())
(defclass mac (platform) ())
(defclass freebsd (platform) ())

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
                ("FreeBSD"
                 (make-instance 'freebsd))
                ("Windows"
                 (make-instance 'windows))
                (otherwise
                 (error "unsupported platform: ~A" (sdl2:platform))))))))
