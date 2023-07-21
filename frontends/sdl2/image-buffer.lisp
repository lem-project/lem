(defpackage :lem-sdl2/image-buffer
  (:use :cl
        :lem
        :lem-sdl2))
(in-package :lem-sdl2/image-buffer)

(defclass image-buffer (text-buffer) ())

(defun buffer-image (buffer)
  (buffer-value buffer 'image))

(defun (setf buffer-image) (image buffer)
  (setf (buffer-value buffer 'image) image))

(defun buffer-scaling (buffer)
  (buffer-value buffer 'scaling))

(defun (setf buffer-scaling) (scaling buffer)
  (setf (buffer-value buffer 'scaling) scaling))

(define-major-mode image-viewer-mode ()
    (:name "Image Viewer"
     :keymap *image-viewer-keymap*)
  (modeline-add-status-list 'image-information (current-buffer))
  (setf (lem:buffer-read-only-p (current-buffer)) t))

(defun image-information (window)
  (let ((image (buffer-image (window-buffer window))))
    (format nil "  ~Dx~D"
            (lem-sdl2::image-width image)
            (lem-sdl2::image-height image))))

(define-key *image-viewer-keymap* "C-+" 'image-zoom-in)
(define-key *image-viewer-keymap* "+" 'image-zoom-in)
(define-key *image-viewer-keymap* "C--" 'image-zoom-out)
(define-key *image-viewer-keymap* "-" 'image-zoom-out)
(define-key *image-viewer-keymap* "C-0" 'image-zoom-reset)
(define-key *image-viewer-keymap* "0" 'image-zoom-reset)
(define-key *image-viewer-keymap* "?" 'image-zoom-help)
(define-key *image-viewer-keymap* "C-h" 'image-zoom-help)

(defmethod render :before (texture window (buffer image-buffer))
  (sdl2:set-render-target (current-renderer) texture)
  (lem-sdl2::set-render-color lem-sdl2::*display* (lem-sdl2::display-background-color lem-sdl2::*display*))
  (sdl2:with-rects ((rect 0
                          0
                          (* (lem-sdl2::char-width)
                             (window-width window))
                          (* (lem-sdl2::char-height)
                             (1- (window-height window)))))
    (sdl2:render-fill-rect (current-renderer) rect)))

(defun scale-buffer-image (buffer scale-offset)
  (clear-drawables buffer)
  (let ((image (buffer-image buffer)))
    (incf (buffer-scaling buffer) scale-offset)
    (draw-image buffer
                image
                :x 0
                :y 0
                :width (round (* (lem-sdl2::image-width image) (buffer-scaling buffer)))
                :height (round (* (lem-sdl2::image-height image) (buffer-scaling buffer))))))

(defun reset-buffer-scale (buffer)
  (clear-drawables buffer)
  (setf (buffer-scaling buffer) 1)
  (let ((image (buffer-image buffer)))
    (draw-image buffer
                image
                :x 0
                :y 0
                :width (lem-sdl2::image-width image)
                :height (lem-sdl2::image-height image))))

(define-command image-zoom-in () ()
  (scale-buffer-image (current-buffer) 0.1))

(define-command image-zoom-out () ()
  (scale-buffer-image (current-buffer) -0.1))

(define-command image-zoom-reset () ()
  (reset-buffer-scale (current-buffer)))

(define-command image-zoom-help () ()
  (with-pop-up-typeout-window (s (make-buffer "*image-zoom-help*") :erase t)
    (format s "Open an image file in Lem and use these keys to zoom in and out:~&")
    (format s "Zoom in: + or C - + (M-x image-zoom-in)~&")
    (format s "Zoom out: - or C - - (M-x image-zoom-out)~&")
    (format s "Zoom reset: 0 or C - 0 (M-x image-zoom-reset)~&")))

(defclass sdl2-find-file-executor (lem:find-file-executor) ())

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
    (setf (buffer-image buffer) image)
    (setf (buffer-scaling buffer) 1)
    (draw-image buffer image :x 0 :y 0)
    (change-buffer-mode buffer 'image-viewer-mode)
    buffer))

(setf lem:*find-file-executor* (make-instance 'sdl2-find-file-executor))
