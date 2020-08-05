(defpackage :lem-tests/all
  (:nicknames :lem-tests)
  (:use :cl)
  (:import-from :lem-tests/lisp-indent-test)
  (:export :run-all-tests))
(in-package :lem-tests/all)

(defun run-all-tests ()
  (rove:run :lem-tests/lisp-indent-test))
