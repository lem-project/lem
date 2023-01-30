(defpackage :lem-language-server/micros-tests
  (:use :cl
        :testif
        :micros/lsp-api))
(in-package :lem-language-server/micros-tests)

(defmacro with-micros (() &body body)
  `(let ((micros::*buffer-package* (find-package "CL-USER"))
         (micros::*buffer-readtable* *readtable*))
     ,@body))

(test "eval tests"
  (test "return evaluated value"
    (with-micros ()
      (let ((result (eval-for-language-server "(cons 1 2)")))
        (ok (equal "(1 . 2)" (eval-result-value result)))
        (ok (equal "" (eval-result-output result)))
        (ok (null (eval-result-error result))))))
  (test "reader error"
    (with-micros ()
      (let ((result (eval-for-language-server "(cons 1")))
        (ok (null (eval-result-value result)))
        (ok (equal "" (eval-result-output result)))
        (ok (stringp (eval-result-error result))))))
  (test "standard output"
    (with-micros ()
      (let ((result (eval-for-language-server "(write-line \"hello\")")))
        (ok (equal "\"hello\"" (eval-result-value result)))
        (ok (equal (format nil "hello~%") (eval-result-output result)))
        (ok (null (eval-result-error result))))))
  (test "standard output and error"
    (with-micros ()
      (let ((result (eval-for-language-server "(progn (prin1 \"test\") (error \"foo\"))")))
        (ok (null (eval-result-value result)))
        (ok (equal "\"test\"" (eval-result-output result)))
        (ok (equal "foo" (eval-result-error result)))))))
