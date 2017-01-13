(in-package :lem)

(export '(editor-abort))

(define-condition editor-abort (editor-condition)
  ((depth
    :initarg :depth
    :reader editor-abort-depth
    :initform 0))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Abort"))))

(define-condition exit-editor (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))
