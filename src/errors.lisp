(in-package :lem-core)

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

(define-condition move-cursor-error (editor-error)
  ((point :initarg :point
          :reader move-cursor-error-point)))

(define-condition end-of-buffer (move-cursor-error)
  ()
  (:default-initargs :message "End of buffer"))

(define-condition beginning-of-buffer (move-cursor-error)
  ()
  (:default-initargs :message "Beginning of buffer"))
