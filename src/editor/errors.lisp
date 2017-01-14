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
     (princ "Abort" stream))))

(define-condition exit-editor (editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(define-condition editor-interrupt (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (princ "Interrupt" stream))))
