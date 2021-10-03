(defpackage :lem-tests/main
  (:nicknames :lem-tests)
  (:use :cl)
  (:import-from :lem-tests/deftest
                :run-all-tests)
  (:export :run-all-tests))
(in-package :lem-tests/main)
