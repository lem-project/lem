(defpackage :lem-lisp-mode/completion
  (:use :cl :lem-lisp-mode/internal)
  (:export :make-completions-form-string
           :eval-completions
           :make-completion-items
           :symbol-completion
           :region-completion))
(in-package :lem-lisp-mode/completion)

(defun make-completions-form-string (string package-name)
  (format nil
          "(cl:first (micros:fuzzy-completions ~S ~S))"
          string
          package-name))

(defun eval-completions (string package)
  (lisp-eval-from-string (make-completions-form-string string package)
                         "COMMON-LISP-USER"))

(defun make-completion-item* (completion &optional start end)
  (lem/completion-mode:make-completion-item
   :label (first completion)
   :chunks (loop :for (offset substring) :in (third completion)
                 :collect (cons offset (+ offset (length substring))))
   :detail (fourth completion)
   :start start
   :end end))

(defun make-completion-items (completions &rest args)
  (mapcar (lambda (completion)
            (apply #'make-completion-item* completion args))
          completions))

(defun symbol-completion (string &optional (package (current-package)))
  (let ((completions (eval-completions string package)))
    (make-completion-items completions)))

(defun region-completion (start end &optional (package (current-package)))
  (let* ((completions (eval-completions (lem:points-to-string start end)
                                        package)))
    (make-completion-items completions start end)))
