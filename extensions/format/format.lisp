(defpackage #:lem-format
  (:use :cl :lem))
(in-package :lem-format)

(defvar auto-format? nil)

(defgeneric lem-formatter (mode buffer))

(defmacro register-formatter (mode handler)
  `(defmethod lem-formatter ((mode (eql ,mode)) buffer)
     (funcall ,handler buffer)))

(defmacro register-formatters (&body bindings)
  `(progn ,@(mapcar 
             (lambda (binding)
               `(register-formatter
                 ,(first binding) 
                 ,(second binding)))
             bindings)))

(define-command format-buffer (&optional buffer) ()
  (let ((buf (or buffer (current-buffer))))
    (lem-formatter (buffer-major-mode buf) buf)))

(add-hook (variable-value 'after-save-hook :global t)
          (lambda (buffer) (when auto-format? (format-buffer buffer))))