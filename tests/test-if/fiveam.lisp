(defpackage :lem-tests/test-if/fiveam
  (:nicknames :lem-tests/test-if)
  (:use :cl)
  (:export :deftest
           :ok
           :signals
           :testing
           :pass
           :fail
           :run-all-tests))
(in-package :lem-tests/test-if/fiveam)

(defun run-all-tests ()
  (5am:run-all-tests))

(defmacro deftest (name &body body)
  `(5am:test ,name ,@body))

(defmacro ok (form &optional desc)
  (if desc
      `(5am:is-true ,form "~A" ,desc)
      `(5am:is-true ,form)))

(defmacro signals (form &optional (condition 'error))
  (let ((c (gensym))
        (condition-type (gensym)))
    `(let ((,condition-type ,condition))
       (typep (block nil
                (handler-bind ((condition
                                 (lambda (,c)
                                   (when (typep ,c ,condition-type)
                                     (return ,c)))))
                  ,form
                  nil))
              ,condition-type))))

(defmacro testing (desc &body body)
  (declare (ignore desc))
  `(progn ,@body))

(defun pass (desc)
  (5am:pass "~A" desc))

(defun fail (desc)
  (5am:fail "~A" desc))
