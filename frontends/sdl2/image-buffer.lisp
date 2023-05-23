(defpackage :lem-sdl2/image-buffer
  (:use :cl
        :lem
        :lem-sdl2))
(in-package :lem-sdl2/image-buffer)

(define-major-mode image-viewer-mode ()
    (:name "Image Viewer"
     :keymap *image-viewer-keymap*)
  (setf (lem:buffer-read-only-p (current-buffer)) t))

(defclass sdl2-find-file-executor (lem::find-file-executor) ())

(defmethod lem:execute-find-file ((executor sdl2-find-file-executor) mode pathname)
  (cond ((member (pathname-type pathname)
                 '("png" "jpg" "jpeg" "bmp" "gif")
                 :test #'equal)
         (open-image-buffer pathname))
        (t
         (call-next-method))))

(defun open-image-buffer (pathname)
  (let ((image (load-image pathname))
        (buffer (lem:make-buffer (file-namestring pathname))))
    (change-class buffer 'image-buffer)
    (draw-image buffer image :x 0 :y 0)
    buffer))

(setf lem::*find-file-executor* (make-instance 'sdl2-find-file-executor))
