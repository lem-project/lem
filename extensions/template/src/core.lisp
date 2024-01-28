(defpackage #:lem-template/core
  (:nicknames :lem-template)
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:line-up-first #:when-let #:rcurry))
(in-package :lem-template/core)

(defvar *templates* nil
  "List of registered file templates.")

(defparameter *auto-template* t)

(defstruct template
  pattern
  file)

(defun empty-buffer-p (buffer)
  (point= (buffer-start-point buffer)
          (buffer-end-point buffer)))

(defun register-template (&key pattern file)
  (setf *templates*
        (cons (make-template :pattern pattern :file file)
              *templates*)))

(defmacro register-templates (&body templates)
  `(progn
     ,@(mapcar
        (lambda (it) `(register-template ,@it))
        templates)))

(defun render-file (template-file &optional args)
  (line-up-first
   (uiop:read-file-string template-file)
   (cl-template:compile-template)
   (funcall args)))

(defun template-match-p (template filename)
  (cl-ppcre:scan (template-pattern template) filename))

(defun find-match (buffer-filename)
  (when-let ((tmpl (find-if (rcurry #'template-match-p buffer-filename) *templates*)))
    (template-file tmpl)))

(defun insert-template (buffer)
  (when-let (file (find-match (buffer-filename buffer)))
    (insert-string
     (buffer-start-point buffer)
     (render-file file `(:buffer ,buffer)))))

(add-hook *find-file-hook*
          (lambda (buffer)
            (when (and *auto-template*
                       (empty-buffer-p buffer)
                       (not (uiop:file-exists-p (buffer-filename buffer))))
              (insert-template buffer))))
