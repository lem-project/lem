
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

(defvar *vcs-existence-order*
  (list #'lem/porcelain-git:git-project-p
        #'lem/porcelain-fossil:fossil-project-p
        #'lem/porcelain-hg:hg-project-p))

(defun vcs-project-p ()
  "When this project is under a known version control system, returns a VCS object for the project.
   Otherwise, returns nil."
  ;; This doesn't return the 2 values :(
  ;; (or (fossil-project-p)
  ;;     (git-project-p))
  (loop for fn in *vcs-existence-order*
        do (alexandria:if-let (vcs (funcall fn))
             (return vcs))))

(defun call-with-current-project (function)
  (with-porcelain-error ()
    (let ((root (lem-core/commands/project:find-root (lem:buffer-directory))))
      (uiop:with-current-directory (root)
        (alexandria:if-let (vcs (vcs-project-p))
          (funcall function vcs)
          (lem:message "Not inside a version-controlled project?"))))))

(defmacro with-current-project ((vcs-bind) &body body)
  "Execute body with the current working directory changed to the project's root,
  find and `vcs-bind` as the VCS

  If no Git directory (or other supported VCS system) are found, message the user."
  `(call-with-current-project (lambda (,vcs-bind) ,@body)))
