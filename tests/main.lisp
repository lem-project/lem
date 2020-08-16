(defpackage :lem-tests/all
  (:nicknames :lem-tests)
  (:use :cl)
  (:import-from :lem-tests/lisp-indent-test)
  (:import-from :lem-tests/syntax-test)
  (:import-from :lem-tests/buffer-list-test)
  (:export :run-all-tests))
(in-package :lem-tests/all)

(defun run-all-tests ()
  (rove:run :lem-tests/lisp-indent-test)
  (rove:run :lem-tests/syntax-test)
  (rove:run :lem-tests/buffer-list-test))
