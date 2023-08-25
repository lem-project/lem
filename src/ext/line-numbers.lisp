(defpackage :lem/line-numbers
  (:use :cl :lem)
  (:export :line-numbers-attribute
           :line-numbers
           :toggle-line-numbers)
  #+sbcl
  (:lock t))
(in-package :lem/line-numbers)

(defvar *initialized* nil)
(defvar *line-number-format* nil)

(define-attribute line-numbers-attribute
  (t :foreground :base07 :background :base01))

(define-editor-variable line-numbers nil ""
  (lambda (value)
    (line-numbers-mode value)))

(define-minor-mode line-numbers-mode
    (:name "Line numbers"
     :global t))

(define-command toggle-line-numbers () ()
  (line-numbers-mode))

(defmethod lem-core::compute-left-display-area-content ((mode line-numbers-mode) buffer point)
  (when (buffer-filename (point-buffer point))
    (let ((string (format nil "~6D " (line-number-at-point point))))
      (lem-base::make-content :string string
                              :attributes `((0 ,(length string) line-numbers-attribute))))))
