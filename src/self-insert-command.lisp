(in-package :lem)

(define-editor-variable self-insert-before-hook '())
(define-editor-variable self-insert-after-hook '())

(defun get-self-insert-char ()
  (insertion-key-p (last-read-key-sequence)))

(defclass self-insert-advice () ())

(defmethod execute :before (mode (command self-insert-advice) argument)
  (unless (get-self-insert-char)
    (undefined-key)))

(define-command (self-insert (:advice-classes self-insert-advice))
    (&optional (n 1) (char (get-self-insert-char)))
    ("p" (get-self-insert-char))
  (run-hooks (variable-value 'self-insert-before-hook) char)
  (self-insert-aux char n)
  (run-hooks (variable-value 'self-insert-after-hook) char))

(defun self-insert-aux (char n &optional sticky)
  (insert-character (current-point) char n)
  (when sticky
    (character-offset (current-point) (- n))))
