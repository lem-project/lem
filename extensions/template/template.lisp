(defpackage #:lem-template
  (:use :cl :lem)
  (:import-from #:alexandria-2 #:line-up-first #:when-let #:rcurry)
  (:export #:*templates*
           #:*auto-template*
           #:register-template
           #:register-templates
           #:insert-template))
(in-package :lem-template/core)

(defvar *templates* nil
  "List of registered file templates.")

(defparameter *auto-template* t
  "Enable automatically populating new files with templates.")

(defstruct template
  pattern
  file)

(defun remove-old-template (pattern)
  "Get rid of old template for pattern when a new one is registered."
  (setf *templates*
        (remove-if
         (lambda (tmpl) (equal pattern (template-pattern tmpl)))
         *templates*)))

(defun register-template (&key pattern file)
  "Register a template used for filenames matching pattern."
  (remove-old-template pattern)
  (push (make-template :pattern pattern :file file) *templates*))

(defmacro register-templates (&body templates)
  "Register multiple templates with `register-template`."
  `(progn
     ,@(mapcar
        (lambda (it) `(register-template ,@it))
        templates)))

(defun render-file (template-file &optional args)
  "Render a cl-template file to a string."
  (line-up-first
   (uiop:read-file-string template-file)
   (cl-template:compile-template)
   (funcall args)))

(defun template-match-p (template filename)
  "Template pattern matches filename."
  (cl-ppcre:scan (template-pattern template) filename))

(defun find-match (buffer-filename)
  "Find template where pattern matches filename."
  (when-let ((tmpl (find-if (rcurry #'template-match-p buffer-filename) *templates*)))
    (template-file tmpl)))

(defun new-file-p (buffer)
  "Buffer is a new file, and does not already exist on disk."
  (not (uiop:file-exists-p (buffer-filename buffer))))

(define-command insert-template (&optional (buffer (current-buffer))) ()
  "Insert registered template into buffer."
  (when-let (file (find-match (buffer-filename buffer)))
    (handler-case
        (insert-string
         (buffer-start-point buffer)
         (render-file file `(:buffer ,buffer)))
      (error (c)
        (declare (ignore c))
        (message "Failed to render template: ~a" file)))))

(add-hook *find-file-hook*
          (lambda (buffer)
            (when (and *auto-template* (new-file-p buffer))
              (insert-template buffer))))
