(defpackage :lem-tests/tx2-test
  (:use :cl :rove)
  (:import-from :yeditor-tx
    #:install-effect
    #:tx-commit
    #:tx-rollback
    #:*tx-pending*
    #:*tx-committed*
    #:with-transaction))
(in-package :lem-tests/tx2-test)

;; Simple counter for side‑effects
(defvar *counter* 0)

(defun inc-fn () (incf *counter*))
(defun dec-fn () (decf *counter*))

(deftest tx2-commit-rollback
  "Test tx-commit and tx-rollback with multiple effects."
  (let ((*counter* 0))
    (progn
      (with-transaction
        (dotimes (i 5)
          (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)))
      (tx-commit)
      (ok (= *counter* 5))
      (tx-rollback)
      (ok (= *counter* 0)))))

(deftest tx2-all-or-nothing
  "Test that an error aborts all effects."
  (let ((*counter* 0))
    (progn
      (with-transaction
        (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
        (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
        (install-effect :test :bad (lambda () (error "boom")) :undo-thunk #'dec-fn))
      (handler-case
          (tx-commit)
        (error (e) (progn)))
      (ok (= *counter* 0)))))

(deftest tx2-property-random
  "Property‑based test with random effects over 20 seeds."
  (dotimes (seed 20)
    (let ((*counter* 0)
          (rs (make-random-state t)))
      (progn
        (with-transaction
          (dotimes (i 100)
            (let ((choice (random 2 rs)))
              (if (= choice 0)
                  (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
                  (install-effect :test :dec #'dec-fn :undo-thunk #'inc-fn)))))
        (tx-commit)
        (tx-rollback)
        (ok (= *counter* 0))))))
