(defpackage :lem-tests/test-if/rove
  (:nicknames :lem-tests/test-if)
  (:use :cl :rove)
  (:export :deftest
           :ok
           :signals
           :testing
           :pass
           :fail
           :run-all-tests))
(in-package :lem-tests/test-if/rove)

(defun run-all-tests ()
  (rove:run :lem-tests))
