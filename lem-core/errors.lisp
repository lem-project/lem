(in-package :lem)

(export '(editor-abort))

(define-condition editor-abort (editor-condition)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (princ "Abort" stream))))

(define-condition exit-editor (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))
