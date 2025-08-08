(uiop:define-package :lem/image-buffer
  (:use :cl))
(in-package :lem/image-buffer)

(defclass image-buffer (lem:html-buffer) ())

(defparameter *html-template*
  "
<style>
img {
  max-width: 100%;
  height: auto;
}
</style>
<img src=\"{{image}}\" />")

(defun open-image-buffer (pathname)
  (let ((buffer (lem:make-buffer
                 (file-namestring pathname)
                 :directory (lem:expand-file-name
                             (namestring (uiop:pathname-directory-pathname
                                          pathname))))))
    (change-class buffer
                  'image-buffer
                  :html
                  (mustache:render* *html-template*
                                    `((image . ,(format nil
                                                        "/local~A"
                                                        pathname)))))))

(defclass find-file-executor (lem:find-file-executor) ())

(defmethod lem:execute-find-file ((executor find-file-executor) mode pathname)
  (cond ((member (pathname-type pathname)
                 '("png" "jpg" "jpeg" "bmp" "gif")
                 :test #'equal)
         (open-image-buffer pathname))
        (t
         (call-next-method))))

(setf lem:*find-file-executor* (make-instance 'find-file-executor))
