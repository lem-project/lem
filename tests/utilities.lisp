(defpackage :lem-tests/utilities
  (:use :cl)
  (:import-from :cl-ansi-text)
  (:export :sample-file
           :with-global-variable-value
           :diff-text
           :with-testing-buffer))
(in-package :lem-tests/utilities)

(defun sample-file (filename)
  (asdf:system-relative-pathname :lem-tests (merge-pathnames filename "tests/sample-code/")))

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
                          (write-string (cl-ansi-text:cyan (format nil "-~A~%" line2)) out)))))))))

(defmacro with-testing-buffer ((buffer make-buffer-form) &body body)
  `(lem-base::with-current-buffers ()
     (let ((,buffer ,make-buffer-form))
       (setf (lem:current-buffer) ,buffer)
       ,@body)))
