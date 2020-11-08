(defpackage :lem-tests/deftest
  (:use :cl)
  (:import-from :cl-ansi-text)
  (:import-from :alexandria
                :once-only
                :appendf
                :hash-table-alist)
  (:export :deftest
           :testing
           :ok
           :pass
           :fail
           :signals
           :run-all-tests
           :run-test))
(in-package :lem-tests/deftest)

(defvar *test-table* (make-hash-table))
(defvar *level* 0)
(defvar *success* '())
(defvar *failure* '())

(defstruct test-result
  form
  description)

(defstruct (success (:include test-result)))
(defstruct (failure (:include test-result)))

(defstruct test
  name
  function
  package)

(defun indent (level)
  (loop :repeat level :do (write-string "  ")))

(defun register-test (name function)
  (let ((test (find name (gethash *package* *test-table*) :key #'test-name)))
    (if test
        (setf (test-function test) function)
        (appendf (gethash *package* *test-table*)
                 (list (make-test :name name
                                  :function function
                                  :package *package*))))))

(defmacro testing (description &body body)
  (once-only (description)
    `(progn
       (when ,description (print-description ,description))
       (let ((*level* (1+ *level*)))
         ,@body))))

(defmacro deftest (name &body body)
  `(register-test ',name (lambda () ,@body)))

(defun print-description (description)
  (indent *level*)
  (format t "~A~%" description))

(defun ok-aux (form function)
  (let ((result (funcall function)))
    (if result
        (pass (prin1-to-string form))
        (fail (prin1-to-string form)))
    result))

(defmacro ok (form &optional description)
  `(ok-aux ',form
           (lambda ()
             (testing ,description
               ,form))))

(defun print-pass-or-fail (mark description)
  (indent *level*)
  (write-string mark)
  (uiop:println description))

(defun pass (description)
  (push (make-test-result :description description) *success*)
  (print-pass-or-fail (cl-ansi-text:green "✓ ") description))

(defun fail (description)
  (push (make-test-result :description description) *failure*)
  (print-pass-or-fail (cl-ansi-text:red "× ") description))

(defmacro signals (form expected-condition)
  (let ((c (gensym)))
    `(handler-case (progn ,form nil)
       (condition (,c)
         (typep ,c ,expected-condition)))))

(defun get-tests-to-each-packages ()
  (hash-table-alist *test-table*))

(defun run-test-1 (test)
  (testing (string-downcase (test-name test))
    (let ((*package* (test-package test)))
      (funcall (test-function test)))))

(defun run-all-tests ()
  (let ((*success* '())
        (*failure* '()))
    (loop :for (package . tests) :in (get-tests-to-each-packages)
          :do (testing (string-downcase (package-name package))
                (dolist (test tests)
                  (run-test-1 test))))
    (null *failure*)))

(defun find-test (test-name)
  (maphash (lambda (package tests)
             (declare (ignore package))
             (dolist (test tests)
               (when (eq test-name (test-name test))
                 (return-from find-test test))))
           *test-table*))

(defun run-test (test-name)
  (let ((test (find-test test-name)))
    (unless test
      (error "The test does not exists: ~A" test-name))
    (run-test-1 test)))
