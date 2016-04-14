;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(editor-condition
          editor-abort
          readonly
          editor-error))

(define-condition editor-condition (simple-error)
  ())

(define-condition editor-abort (editor-condition)
  ((depth
    :initarg :depth
    :reader editor-abort-depth
    :initform 0)))

(define-condition readonly (editor-condition)
  ())

(define-condition switch-minibuffer-window (editor-condition)
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
