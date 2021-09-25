(in-package :lem)

(export '(self-insert-before-hook
          self-insert-after-hook
          self-insert))

(define-editor-variable self-insert-before-hook '())
(define-editor-variable self-insert-after-hook '())

(defun get-self-insert-char ()
  (insertion-key-p (last-read-key-sequence)))

(defclass self-insert-advice () ())

(defmethod execute :before ((command self-insert-advice) argument)
  (unless (get-self-insert-char)
    (undefined-key)))

(define-command (self-insert (:advice-classes self-insert-advice)) (char &optional (n 1))
    ((list (get-self-insert-char)) "p")
  (run-hooks (variable-value 'self-insert-before-hook) char)
  (insert-character (current-point) char n)
  (run-hooks (variable-value 'self-insert-after-hook) char))
