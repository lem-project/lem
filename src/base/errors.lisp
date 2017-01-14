(in-package :lem-base)

(export '(editor-condition
          read-only-error
          editor-error
          editor-error-message
          scan-error))

(define-condition editor-condition (simple-error)
  ())

(define-condition read-only-error (editor-condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (princ "Read Only" stream))))

(define-condition editor-error (editor-condition)
  ((message
    :initform ""
    :initarg :message
    :reader editor-error-message))
  (:report
   (lambda (condition stream)
     (princ (editor-error-message condition) stream))))

(defun editor-error (message &rest args)
  (error 'editor-error :message (apply #'format nil message args)))

(defun scan-error ()
  (editor-error "Scan Error"))
