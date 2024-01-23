(defpackage #:lem-format
  (:use :cl :lem)
  (:export #:*auto-format*
           #:register-formatter 
           #:register-formatters 
           #:format-buffer))
(in-package :lem-format)

;; Set this to true to format on save.
(defvar *auto-format* nil)

;; Formatter methods for lem-format.  
;; You don't need to use this directly, the `register-formatter` will do it for you.
(defgeneric lem-formatter (mode buffer))

(defmacro register-formatter (mode handler)
  "Register a formatter for a mode, handler takes buffer as argument."
  `(defmethod lem-formatter ((mode (eql (quote ,mode))) buffer)
     (funcall ,handler buffer)))

(defmacro register-formatters (&body bindings)
  "Register multiple formatters at once."
  `(progn ,@(mapcar 
             (lambda (binding)
               `(register-formatter
                 ,(first binding) 
                 ,(second binding)))
             bindings)))

(defun save-without-hooks (buffer)
  "Bypass hooks to avoid infinite looping."
  (lem/buffer/file::write-to-file-1 buffer (buffer-filename buffer)))

(define-command format-buffer (&key buffer auto) ()
  "Try to format a buffer."
  (let* ((buf (or buffer (current-buffer)))
         (mode (buffer-major-mode buf)))
    (unless auto (save-without-hooks buf))
    (handler-case (lem-formatter mode buf)
      (error (c) 
        (declare (ignore c))
        (unless auto (message "No formatter for mode ~a" mode))))
    (save-without-hooks buf)))

;; When `*auto-format*` is true, try to format a buffer when it is saved.
(add-hook (variable-value 'after-save-hook :global t)
          (lambda (buffer) 
            (when *auto-format* 
              (format-buffer :buffer buffer :auto t))))
