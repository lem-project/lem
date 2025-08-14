(uiop:define-package :lem/image-buffer
  (:use :cl))
(in-package :lem/image-buffer)

(lem:define-major-mode image-viewer-mode ()
    (:name "Image Viewer"
     :keymap *image-viewer-keymap*)
  (setf (lem:buffer-read-only-p (lem:current-buffer)) t))

(lem:define-key *image-viewer-keymap* "+" 'image-zoom-in)
(lem:define-key *image-viewer-keymap* "-" 'image-zoom-out)
(lem:define-key *image-viewer-keymap* "0" 'image-zoom-reset)
(lem:define-key *image-viewer-keymap* "?" 'image-usage)

(defclass image-buffer (lem:html-buffer) ())

(defparameter *html-template*
  "
<style>
img {
  transition: width 0.2s ease, height 0.2s ease;
}
</style>
<img id='image' src=\"/local{{image}}\" />

<p> Press '?' to usage </p>
<script>

const img = document.getElementById('image');
img.width = window.innerWidth;

let currentWidth = img.width;

function scaleImage(factor) {
  currentWidth *= factor;
  img.width = currentWidth;
}

function resetImage() {
  currentWidth = window.innerWidth;
  img.width = currentWidth;
}
</script>")

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
                                    `((image . ,(namestring pathname)))))
    (lem:change-buffer-mode buffer 'image-viewer-mode)))

(defclass find-file-executor (lem:find-file-executor) ())

(defmethod lem:execute-find-file ((executor find-file-executor) mode pathname)
  (cond ((and (lem:html-support-p (lem:implementation))
              (member (pathname-type pathname)
                      '("png" "jpg" "jpeg" "bmp" "gif")
                      :test #'equal))
         (open-image-buffer pathname))
        (t
         (call-next-method))))

(lem:define-command image-zoom-in () ()
  (lem:js-eval (lem:current-window) "scaleImage(1.2)"))

(lem:define-command image-zoom-out () ()
  (lem:js-eval (lem:current-window) "scaleImage(0.8)"))

(lem:define-command image-zoom-reset () ()
  (lem:js-eval (lem:current-window) "resetImage()"))

(lem:define-command image-viewer-help () ()
  (lem:with-pop-up-typeout-window
      (s (lem:make-buffer "*image-help*" :temporary t) :erase t)
    (format s "Open an image file in Lem and use these keys to zoom in and out.~2&")
    (format s "Zoom in: Alt-x image-zoom-in (~{~A~^, ~})~&"
            (lem/prompt-window:find-command-keybindings-in-keymap "image-zoom-in" *image-viewer-keymap*))
    (format s "Zoom out: Alt-x image-zoom-out (~{~A~^, ~})~&"
            (lem/prompt-window:find-command-keybindings-in-keymap "image-zoom-out" *image-viewer-keymap*))
    (format s "Reset: Alt-x image-zoom-reset (~{~A~^, ~})~&"
            (lem/prompt-window:find-command-keybindings-in-keymap "image-zoom-reset" *image-viewer-keymap*))))

(setf lem:*find-file-executor* (make-instance 'find-file-executor))
