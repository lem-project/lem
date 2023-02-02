(defpackage :lem-tests/lisp-syntax/indent-test
  (:use :cl
        :rove)
  (:import-from :lem-base)
  (:import-from :lem-lisp-syntax)
  (:import-from :lem-lisp-mode)
  (:import-from :lem-tests/utilities
                :sample-file
                :diff-text))
(in-package :lem-tests/lisp-syntax/indent-test)

(defmacro define-indent-test (name before &optional (after before))
  `(deftest ,name
     (let ((lem-lisp-mode::*disable-self-connect* t))
       (run-indent-test ,(string name) ,before ,after))))

(defun run-indent-test (name before-text after-text)
  (let ((buffer (lem-base:make-buffer (format nil "*indent-test ~A*" name)
                                      :syntax-table lem-lisp-syntax:*syntax-table*
                                      :temporary t)))
    (setf (lem-base:variable-value 'lem-base:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem-base:erase-buffer buffer)
    (lem-base:with-point ((p (lem-base:buffer-point buffer)))
      (lem-base:insert-string p before-text))
    (lem-base:indent-buffer buffer)
    (unless (ok (string= after-text (lem-base:buffer-text buffer)) name)
      (report name
              (lem-base:buffer-text buffer)
              after-text))))

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

(defun get-location-from-buffer-point (point pathname)
  (format nil "~A:~D:~A" pathname (lem-base:line-number-at-point point) (lem-base:line-string point)))

(defun indent-test-for-file (pathname)
  (let ((buffer (lem-base:find-file-buffer pathname :temporary t :enable-undo-p nil)))
    (setf (lem-base:variable-value 'lem-base:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem-base:with-point ((p (lem-base:buffer-point buffer)))
      (lem-base:buffer-start p)
      (loop
        (lem-base:with-point ((start p))
          (unless (lem-base:form-offset p 1)
            (return))
          (lem-base:skip-space-and-comment-forward p)
          (let ((text (lem-base:points-to-string start p))
                (name (get-location-from-buffer-point start pathname)))
            (run-indent-test name text text)))))))

(defun indent-test-for-system (system-name)
  (dolist (pathname (directory
                     (merge-pathnames "*.lisp"
                                      (asdf:system-source-directory system-name))))
    (indent-test-for-file pathname)))

(deftest indent-test-under-lem-base
  (let ((lem-lisp-mode::*disable-self-connect* t))
    (indent-test-for-system :lem-base)))

(deftest indent-test-for-sample-case
  (let ((lem-lisp-mode::*disable-self-connect* t))
    (indent-test-for-file
     (sample-file "indent-sample.lisp"))))

(lem-lisp-syntax:indentation-update)
