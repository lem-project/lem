(in-package :lem-tests)

(defmacro define-indent-test (name before &optional (after before))
  (let ((name (if (eq name '*)
                  (generate-anonymous-test-name "INDENT-")
                  name)))
    `(define-test ,name
       (run-indent-test ',name ,before ,after))))

(defun run-indent-test (name before-text after-text)
  (let ((buffer (lem:make-buffer (format nil "*indent-test ~A*" name)
                                 :syntax-table lem-lisp-syntax:*syntax-table*
                                 :temporary t)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem:erase-buffer buffer)
    (lem:with-point ((p (lem:buffer-point buffer)))
      (lem:insert-string p before-text))
    (lem:indent-buffer buffer)
    (test (string= after-text (lem:buffer-text buffer))
          (report name
                  (lem:buffer-text buffer)
                  after-text))))

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

(defun report (name before-text after-text)
  (format nil "# indentation error: ~A~%~A~%"
          name
          (diff-text before-text after-text)))


(define-indent-test cond-1
"
(cond ((foo 1
            2)))
")

(define-indent-test defclass-1
  "
`(defclass foo ()
   ,(mapcar x
            y))
")

(defun indent-test-for-file (pathname)
  (let ((buffer (lem:find-file-buffer pathname :temporary t :enable-undo-p nil)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem:with-point ((p (lem:buffer-point buffer)))
      (lem:buffer-start p)
      (loop
        (lem:with-point ((start p))
          (unless (lem:form-offset p 1)
            (return))
          (let ((text (lem:points-to-string start p))
                (name (format nil "~A:~D" pathname (lem:line-number-at-point start))))
            (run-indent-test name text text)))))))

(defun indent-test-for-system (system-name)
  (dolist (pathname (directory
                     (merge-pathnames "*.lisp"
                                      (asdf:system-source-directory system-name))))
    (indent-test-for-file pathname)))

(define-test indent-test-under-lem-base
  (indent-test-for-system :lem-base))

(define-test indent-test-for-sample-case
  (indent-test-for-file
   (asdf:system-relative-pathname :lem
                                  "./tests/sample-code/indent-sample.lisp")))
