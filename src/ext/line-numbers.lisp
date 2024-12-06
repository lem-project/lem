(defpackage :lem/line-numbers
  (:use :cl :lem)
  (:export :*relative-line*
           :line-number-format
           :custom-current-line
           :line-numbers-attribute
           :active-line-number-attribute
           :line-numbers
           :toggle-line-numbers)
  #+sbcl
  (:lock t))
(in-package :lem/line-numbers)

(defparameter *relative-line* nil
  "Set to t to show relative line numbers by default. Also use a prefix argument to `toggle-line-numbers'.")

(defvar *previous-relative-line* nil)

(defvar *initialized* nil)

(define-editor-variable line-number-format "~6D "
  "Set to desired format, for example, \"~2D \" for a
two-character line-number column.")

(define-editor-variable custom-current-line nil
  "Set to desired current-line value when relative line
numbers are active, for example, \"->\".  NIL will make the
the absolute value of the current line display.")

(define-attribute line-numbers-attribute
  (t :foreground :base07 :background :base01))

(define-attribute active-line-number-attribute
  (t :foreground :base07 :background :base01))

(define-editor-variable line-numbers nil ""
  (lambda (value)
    (line-numbers-mode value)))

(define-minor-mode line-numbers-mode
    (:name "Line numbers"
     :global t
     :disable-hook 'disable-hook))

(define-command toggle-line-numbers (&optional relative) (:universal-nil)
  "Toggle the display of line numbers.

With a positive universal argument, use relative line numbers. Also obey the global variale `*relative-line*'."
  (setf *previous-relative-line* *relative-line*
        *relative-line* (or *relative-line*
                            (and relative (plusp relative))))
  (line-numbers-mode))

(defun disable-hook ()
  (setf *relative-line* *previous-relative-line*))

(defun compute-line (buffer point)
  (if *relative-line*
      (let ((cursor-line (line-number-at-point (buffer-point buffer)))
            (line (line-number-at-point point)))
        (if (= cursor-line line)
            (if (variable-value 'custom-current-line)
                (variable-value 'custom-current-line :default buffer)
                line)
            (abs (- cursor-line line))))
      (line-number-at-point point)))

(defmethod lem-core:compute-left-display-area-content ((mode line-numbers-mode) buffer point)
  (when (buffer-filename (point-buffer point))
    (let* ((string (format nil (variable-value 'line-number-format :default buffer) (compute-line buffer point)))
           (attribute (if (eq (compute-line buffer point)
                              (compute-line buffer (buffer-point buffer)))
                          `((0 ,(length string) active-line-number-attribute))
                          `((0 ,(length string) line-numbers-attribute)))))
      (lem/buffer/line:make-content :string string
                                    :attributes attribute))))
