;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(define-condition lem-error (simple-error)
  ())

(define-condition editor-abort (lem-error)
  ((depth
    :initarg :depth
    :reader editor-abort-depth
    :initform 0)))

(define-condition readonly (lem-error)
  ())
