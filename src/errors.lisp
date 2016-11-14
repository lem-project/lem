(in-package :lem)

(export '(editor-condition
          editor-abort
          editor-abort-depth
          readonly
          editor-error
          editor-error-message))

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

(define-condition readonly (editor-condition)
  ())

(define-condition editor-error (editor-condition)
  ((message
    :initarg :message
    :reader editor-error-message))
  (:report
   (lambda (condition stream)
     (format stream
             "Editor Error: ~A"
             (editor-error-message condition)))))

(defun editor-error (message &rest args)
  (error 'editor-error :message (apply #'format nil message args)))
