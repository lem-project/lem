(defpackage :lem-tests/utilities
  (:use :cl)
  (:import-from :cl-ansi-text)
  (:export :sample-file
           :with-global-variable-value
           :diff-text
           :with-mock-functions))
(in-package :lem-tests/utilities)

(defun sample-file (filename)
  (asdf:system-relative-pathname :lem-tests (merge-pathnames filename "tests/sample-code/")))

(defun call-with-global-variable-value (var value function)
  (let* ((editor-variable (get var 'lem-base::editor-variable))
         (save-value (lem-base::editor-variable-value editor-variable)))
    (setf (lem-base::editor-variable-value editor-variable) value)
    (unwind-protect (funcall function)
      (setf (lem-base::editor-variable-value editor-variable) save-value))))

(defmacro with-global-variable-value ((var value) &body body)
  `(call-with-global-variable-value ',var ,value (lambda () ,@body)))

(defun diff-text (text1 text2)
  (string-trim
   '(#\newline #\space #\tab)
   (with-output-to-string (out)
     (with-input-from-string (in1 text1)
       (with-input-from-string (in2 text2)
         (loop :with eof-value := '#:eof
               :for line1 := (read-line in1 nil eof-value)
               :for line2 := (read-line in2 nil eof-value)
               :until (eq line1 eof-value)
               :do (cond ((string= line1 line2)
                          (format out " ~A~%" line1))
                         (t
                          (write-string (cl-ansi-text:yellow (format nil "+~A~%" line1)) out)
                          (write-string (cl-ansi-text:cyan (format nil "-~A~%" line2)) out))
                         #+(or)
                         (t
                          (write-string (format nil "+~A~%" line1) out)
                          (write-string (format nil "-~A~%" line2) out)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-with-mock-functions (definitions body)
    (let ((setf-function-forms '())
          (bindings '()))
      (dolist (definition definitions)
        (destructuring-bind (name lambda-list &body body) definition
          (push (list (gensym (string name)) `(symbol-function ',name)) bindings)
          (push `(setf (symbol-function ',name)
                       (lambda ,lambda-list ,@body))
                setf-function-forms)))
      `(let ,bindings
         ,@setf-function-forms
         (unwind-protect (progn ,@body)
           ,@(mapcar (lambda (binding)
                       `(setf ,(second binding) ,(first binding)))
                     bindings))))))

(defmacro with-mock-functions ((&rest definitions) &body body)
  (expand-with-mock-functions definitions body))
