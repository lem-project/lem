(defpackage :lem-tests/all
  (:nicknames :lem-tests)
  (:use :cl)
  (:import-from :lem-tests/utilities
                :run-all-tests)
  (:import-from :lem-tests/conditions)
  (:import-from :lem-tests/lisp-indent-test)
  (:export :run-all-tests))
(in-package :lem-tests/all)
