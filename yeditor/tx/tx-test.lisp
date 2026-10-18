(defpackage :lem-tests/tx-test
  (:use :cl :rove)
  (:import-from :yeditor-tx
    :with-transaction
    :install-effect
    :tx-commit
    :tx-rollback
    :*tx-pending*
    :*tx-committed*))
(in-package :lem-tests/tx-test)

(deftest tx-100-effects
  "Test transaction of 100 fake effects results in 100 committed entries."
  (let ((*tx-pending* nil)
        (*tx-committed* nil))
    (with-transaction
      (dotimes (i 100)
        (install-effect :test :noop (lambda () nil) :undo-thunk (lambda () nil))))
    (tx-commit)
    (ok (= (length *tx-committed*) 100))))
