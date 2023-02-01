(defpackage :lem-language-server/micros-tests
  (:use :cl
        :testif
        :micros/lsp-api))
(in-package :lem-language-server/micros-tests)

(defmacro with-micros-syntax (() &body body)
  `(let ((micros::*buffer-package* (find-package "CL-USER"))
         (micros::*buffer-readtable* *readtable*))
     ,@body))

(test "eval tests"
  (test "return evaluated value"
    (with-micros-syntax ()
      (let ((result (eval-for-language-server "(cons 1 2)")))
        (ok (equal "(1 . 2)" (eval-result-value result)))
        (ok (equal "" (eval-result-output result)))
        (ok (null (eval-result-error result))))))
  (test "reader error"
    (with-micros-syntax ()
      (let ((result (eval-for-language-server "(cons 1")))
        (ok (null (eval-result-value result)))
        (ok (equal "" (eval-result-output result)))
        (ok (stringp (eval-result-error result))))))
  (test "standard output"
    (with-micros-syntax ()
      (let ((result (eval-for-language-server "(write-line \"hello\")")))
        (ok (equal "\"hello\"" (eval-result-value result)))
        (ok (equal (format nil "hello~%") (eval-result-output result)))
        (ok (null (eval-result-error result))))))
  (test "standard output and error"
    (with-micros-syntax ()
      (let ((result (eval-for-language-server "(progn (prin1 \"test\") (error \"foo\"))")))
        (ok (null (eval-result-value result)))
        (ok (equal "\"test\"" (eval-result-output result)))
        (ok (equal "foo" (eval-result-error result)))))))

(defun call-with-micros-connection (function)
  (let ((connection (micros/client:start-server-and-connect nil)))
    (unwind-protect (funcall function connection)
      (micros/client:stop-server connection))))

(defmacro with-micros-connection ((connection) &body body)
  `(call-with-micros-connection (lambda (,connection) ,@body)))

(test "simple eval test"
  (with-micros-connection (connection)
    (let ((result
            (micros/client:remote-eval-sync connection
                                            `(micros:interactive-eval "(cons 1 2)"))))
      (ok (equal "=> (1 . 2)" result)))))
