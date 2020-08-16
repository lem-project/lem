(defpackage :lem-tests/utilities
  (:use :cl)
  (:export :sample-file
           :with-global-variable-value))
(in-package :lem-tests/utilities)

(defun sample-file (filename)
  (asdf:system-relative-pathname :lem (merge-pathnames filename "tests/sample-code/")))

(defun call-with-global-variable-value (var value function)
  (let* ((editor-variable (get var 'lem-base::editor-variable))
         (save-value (lem-base::editor-variable-value editor-variable)))
    (setf (lem-base::editor-variable-value editor-variable) value)
    (unwind-protect (funcall function)
      (setf (lem-base::editor-variable-value editor-variable) save-value))))

(defmacro with-global-variable-value ((var value) &body body)
  `(call-with-global-variable-value ',var ,value (lambda () ,@body)))
