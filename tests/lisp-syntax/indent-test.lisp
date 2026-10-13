(defpackage :lem-tests/lisp-syntax/indent-test
  (:use :cl
        :rove)
  (:import-from :lem)
  (:import-from :lem-lisp-syntax)
  (:import-from :lem-lisp-mode)
  (:import-from :lem-tests/utilities
                :sample-file
                :diff-text))
(in-package :lem-tests/lisp-syntax/indent-test)

(defmacro define-indent-test (name before &optional (after before))
  `(deftest ,name
     (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
       (run-indent-test ,(string name) ,before ,after))))

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
    (unless (ok (string= after-text (lem:buffer-text buffer)) name)
      (report name
              (lem:buffer-text buffer)
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

(deftest defpackage-use-clause-not-affected-by-use-macro
  ;; A user-defined macro named USE must not influence the indentation of
  ;; the :USE clause keyword in DEFPACKAGE. The package prefix of a
  ;; package-qualified symbol may be stripped to look up its indentation,
  ;; but a keyword such as :USE has no package prefix and must be left alone.
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (lem-lisp-syntax.indent:set-indentation "use" '(&body))
    (unwind-protect
         (run-indent-test "defpackage-use-clause-not-affected-by-use-macro"
"
(defpackage :foo
  (:use :cl
        :bar))
"
"
(defpackage :foo
  (:use :cl
        :bar))
")
      (lem-lisp-syntax.indent:set-indentation "use" nil))))

(defun get-location-from-buffer-point (point pathname)
  (format nil "~A:~D:~A" pathname (lem:line-number-at-point point) (lem:line-string point)))

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
          (lem:skip-space-and-comment-forward p)
          (let ((text (lem:points-to-string start p))
                (name (get-location-from-buffer-point start pathname)))
            (run-indent-test name text text)))))))

(defun indent-test-for-system (system-name)
  (dolist (pathname (directory
                     (merge-pathnames "*.lisp"
                                      (asdf:system-source-directory system-name))))
    (indent-test-for-file pathname)))

#+TODO
(deftest indent-test-under-lem
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (indent-test-for-system :lem)))

(deftest indent-test-for-sample-case
  (let ((lem-lisp-mode/test-api:*disable-self-connect* t))
    (indent-test-for-file
     (sample-file "indent-sample.lisp"))))

(lem-lisp-syntax:indentation-update)
