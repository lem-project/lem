(in-package :lem-core)

(define-editor-variable highlight-line t)

(defun highlight-line-color ()
  (when (background-color)
    (let ((color (parse-color (background-color))))
      (multiple-value-bind (h s v)
          (rgb-to-hsv (color-red color)
                      (color-green color)
                      (color-blue color))
        (multiple-value-bind (r g b)
            (hsv-to-rgb h
                        s
                        (max 0 (- v 2)))
          (format nil "#~2,'0X~2,'0X~2,'0X" r g b))))))
