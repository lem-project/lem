(in-package :lem)

(export '(editor-abort))

(defparameter *default-editor-abort-message* "Abort")

(define-condition editor-abort (editor-condition)
  ((message
    :initarg :message
    :initform *default-editor-abort-message*
    :reader editor-abort-message))
  (:report
   (lambda (condition stream)
     (when (editor-abort-message condition)
       (princ (editor-abort-message condition) stream)))))

(define-condition exit-editor ()
  ((report :initarg :report
           :reader exit-editor-report)))
