(in-package :lem-base)

(export '(editor-condition
          editor-abort
          editor-abort-depth
          read-only-error
          editor-error
          editor-error-message
          scan-error))

(define-condition editor-condition (simple-error)
  ())

(define-condition editor-abort (editor-condition)
  ((depth
    :initarg :depth
    :reader editor-abort-depth
    :initform 0))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Abort"))))

(define-condition read-only-error (editor-condition)
  ())

(define-condition editor-error (editor-condition)
  ((message
    :initform ""
    :initarg :message
    :reader editor-error-message))
  (:report
   (lambda (condition stream)
     (format stream
             "Editor Error: ~A"
             (editor-error-message condition)))))

(defun editor-error (message &rest args)
  (error 'editor-error :message (apply #'format nil message args)))

(defun scan-error ()
  (editor-error "Scan Error"))
