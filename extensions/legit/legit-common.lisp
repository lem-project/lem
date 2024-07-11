
(defpackage :lem/legit
  (:use :cl
   :lem)
  (:export :legit-status
           :*prompt-for-commit-abort-p*
           :*ignore-all-space*)
  (:documentation "Display version control data of the current project in an interactive two-panes window.

  This package in particular defines the right window of the legit interface and the user-level commands.

  Gets VCS data by calling lem/porcelain and asking lem/peek-legit to display data on the left window."))

(in-package :lem/legit)

(defun call-with-porcelain-error (function)
  (handler-bind ((lem/porcelain:porcelain-error
                   (lambda (c)
                     (lem:editor-error (slot-value c 'message)))))
    (funcall function)))

(defmacro with-porcelain-error (&body body)
  "Handle porcelain errors and turn them into a lem:editor-error."
  ;; Doing this helps avoiding tight coupling between the porcelain package and Lem.
  `(call-with-porcelain-error (lambda () ,@body)))

(defun call-with-current-project (function)
  (with-porcelain-error ()
    (let ((root (lem-core/commands/project:find-root (lem:buffer-directory))))
      (uiop:with-current-directory (root)
        (multiple-value-bind (root vcs)
            (lem/porcelain:vcs-project-p)
          (if root
                (progn
                  (funcall function vcs))
              (lem:message "Not inside a version-controlled project?")))))))

(defmacro with-current-project ((vcs-bind) &body body)
  "Execute body with the current working directory changed to the project's root,
  find and `vcs-bind` as the VCS

  If no Git directory (or other supported VCS system) are found, message the user."
  `(call-with-current-project (lambda (,vcs-bind) ,@body)))
