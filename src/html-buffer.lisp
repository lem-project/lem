(in-package :lem-core)

(defclass html-buffer (text-buffer)
  ((html :initarg :html
         :accessor html-buffer-html)))

(defun make-html-buffer (buffer-name html)
  (let ((buffer (make-buffer buffer-name)))
    (change-class buffer 'html-buffer :html html)
    buffer))

(defun js-eval (window code)
  (lem-if:js-eval (implementation)
                  (window-view window)
                  code))
