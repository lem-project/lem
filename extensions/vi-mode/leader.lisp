(defpackage :lem-vi-mode/leader
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :vi-keymap)
  (:export :leader-key))
(in-package :lem-vi-mode/leader)

(define-named-key "Leader")

(define-editor-variable leader-key "\\")

(defun mapleader-key ()
  (first (lem-core::parse-keyspec (variable-value 'leader-key))))

(defun mapleader-key-p (key)
  (eq key (mapleader-key)))

(defun leader-key ()
  (make-key :sym "Leader"))

(defmethod keymap-find ((keymap vi-keymap) (key lem-core::key))
  (if (mapleader-key-p key)
      (call-next-method keymap (leader-key))
      (call-next-method)))

(defmethod keymap-find ((keymap vi-keymap) (key cons))
  (if (mapleader-key-p (first key))
      (call-next-method keymap (cons (leader-key) (rest key)))
      (call-next-method)))
