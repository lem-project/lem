(defpackage #:lem-format
  (:use :cl :lem)
  (:export #:auto-format? 
           #:register-formatter 
           #:register-formatters 
           #:format-buffer))
(in-package :lem-format)

(defvar auto-format? nil)

(defgeneric lem-formatter (mode buffer))

(defmacro register-formatter (mode handler)
  "Register a formatter for a mode, handler takes buffer as argument."
  `(defmethod lem-formatter ((mode (eql ,mode)) buffer)
     (funcall ,handler buffer)))

(defmacro register-formatters (&body bindings)
  "Register multiple formatters at once."
  `(progn ,@(mapcar 
             (lambda (binding)
               `(register-formatter
                 ,(first binding) 
                 ,(second binding)))
             bindings)))

(define-command format-buffer (&optional buffer) ()
  "Try to format a buffer."
  (let* ((buf (or buffer (current-buffer)))
         (mode (buffer-major-mode buf)))
    (handler-case (lem-formatter mode buf)
      (error (c) 
        (declare (ignore c))
        (message "No formatter for mode ~a" mode)))))

;; when auto-format? is true, try to format a buffer when it is saved
 (add-hook (variable-value 'after-save-hook :global t)
          (lambda (buffer) (when auto-format? (format-buffer buffer))))
