(uiop:define-package :lem/directory-mode/utils
  (:use :cl)
  (:export :run-command
           :symbolic-link-p
           :pathname-directory-last-name))
(in-package :lem/directory-mode/utils)

(defun run-command (command)
  (when (consp command)
    (setf command (mapcar #'princ-to-string command)))
  (let ((error-string
          (with-output-to-string (error-output)
            (uiop:run-program command
                              :ignore-error-status t
                              :error-output error-output))))
    (when (string/= error-string "")
      (lem:editor-error "~A" error-string))))

(defun symbolic-link-p (pathname)
  (not (uiop:pathname-equal pathname (probe-file pathname))))

(defun pathname-directory-last-name (pathname)
  (enough-namestring pathname (uiop:pathname-parent-directory-pathname pathname)))
