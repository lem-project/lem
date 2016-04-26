(in-package :cl-user)

(defpackage :lem-internal
  (:use :cl)
  (:export
   ;; errors.lisp
   :editor-condition
   :editor-abort
   :editor-abort-depth
   :readonly
   :switch-minibuffer-window
   :editor-error
   :editor-error-message))

(defpackage :lem
  (:use :cl :lem.fatstring :lem.util :lem-internal :lem.term))

(defpackage :lem-user
  (:use :cl :lem))
