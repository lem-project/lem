(defpackage :lem.language-mode-tools
  (:use :cl :lem)
  (:export :make-tm-string-region)
  #+sbcl
  (:lock t))
(in-package :lem.language-mode-tools)

(defun make-tm-string-region (sepalator &key (name 'syntax-string-attribute)
                                             (patterns (make-tm-patterns (make-tm-match "\\\\."))))
  (make-tm-region `(:sequence ,sepalator)
                  `(:sequence ,sepalator)
                  :name name 
                  :patterns patterns))
