(defpackage #:lem-template
  (:use :cl :lem)
  (:import-from #:lem-template/render
                #:render-file)
  (:import-from #:lem-template/prompt
                #:prompt-hash-table)
  (:import-from #:lem-template/utils
                #:buffer-empty-p
                #:new-file-p
                #:hash-table-first)
  (:import-from #:alexandria-2
                #:when-let
                #:rcurry
                #:if-let)
  (:export #:*auto-template*
           #:register-template
           #:register-templates
           #:insert-template
           #:render-file
           #:render-string))
(in-package :lem-template)

(defvar *patterns* nil
  "List of registered file patterns.")

(defparameter *auto-template* t
  "Enable automatically populating new files with templates.")

(defstruct pattern
  pattern
  templates)

(defun register-template (&key pattern file (name "default"))
  "Register a template used for filenames matching pattern."
  (if-let ((p (find-if (lambda (it) (equal pattern (pattern-pattern it))) *patterns*)))
    (setf (gethash name (pattern-templates p)) file)
    (progn (push (make-pattern :pattern pattern
                               :templates (make-hash-table :test #'equal))
                 *patterns*)
           (register-template :pattern pattern :file file :name name))))

(defmacro register-templates (&body templates)
  "Register multiple templates with `register-template`."
  `(progn ,@(mapcar (lambda (it) `(register-template ,@it)) templates)))

(defun template-match-p (template filename)
  "Template pattern matches filename."
  (cl-ppcre:scan (pattern-pattern template) filename))

(defun find-match (buffer-filename)
  "Find template where pattern matches filename."
  (when-let ((p (find-if (rcurry #'template-match-p buffer-filename) *patterns*)))
    (let ((tmpls (pattern-templates p)))
      (if (= 1 (hash-table-count tmpls))
          (hash-table-first tmpls)
          (prompt-hash-table "Template: " tmpls)))))

(defun insert-template (buffer)
  "Insert registered template into buffer."
  (when-let (file (find-match (buffer-filename buffer)))
    (handler-case
        (insert-string
         (buffer-start-point buffer)
         (render-file file `(:buffer ,buffer
                             :path ,(buffer-filename buffer))))
      (error (c)
        (message "Render error: ~a" c)
        (message "Failed to render template: ~a" file)))))

(add-hook *find-file-hook*
          (lambda (buffer)
            (when (and *auto-template* (new-file-p buffer) (buffer-empty-p buffer))
              (insert-template buffer))))
