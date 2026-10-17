(defpackage :lem-tests/tx-test
  (:use :cl :rove)
  (:import-from :yeditor-tx
                :with-transaction
                :install-effect
                :*tx-log*))
(in-package :lem-tests/tx-test)

(deftest tx-100-effects
  "Test transaction of 100 fake effects results in 100 log entries."
  (let ((*tx-log* nil))
    (with-transaction
      (dotimes (i 100)
        (install-effect :noop nil nil #'identity)))
    (ok (= (length *tx-log*) 100))))
