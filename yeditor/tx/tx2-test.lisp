(load "/home/yuri/Documents/yeditor/yeditor/tx/tx.lisp")

(defpackage :yeditor-tx-tests
  (:use :cl)
  (:import-from :yeditor-tx
                #:install-effect
                #:tx-commit
                #:tx-rollback
                #:*tx-pending*
                #:*tx-committed*
                #:with-transaction))
(in-package :yeditor-tx-tests)

;; Simple counter for side‑effects
(defvar *counter* 0)

(defun inc-fn () (incf *counter*))
(defun dec-fn () (decf *counter*))

(defun run-test-commit-rollback ()
  (let ((*counter* 0))
    (with-transaction
      (dotimes (i 5)
        (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)))
    (tx-commit)
    (assert (= *counter* 5))
    (tx-rollback)
    (assert (= *counter* 0))))

(defun run-test-all-or-nothing ()
  (let ((*counter* 0))
    (with-transaction
      (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
      (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
      (install-effect :test :bad (lambda () (error "boom")) :undo-thunk #'dec-fn))
    (handler-case
        (tx-commit)
      (error (e) (progn)))
    (assert (= *counter* 0))))

(defun run-test-property-random ()
  (dotimes (seed 20)
    (let ((*counter* 0)
          (rs (make-random-state nil)))
      (with-transaction
        (dotimes (i 100)
          (let ((choice (random 2 rs)))
            (if (= choice 0)
                (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
                (install-effect :test :dec #'dec-fn :undo-thunk #'inc-fn)))))
      (tx-commit)
      (tx-rollback)
      (assert (= *counter* 0)))))

(defun main ()
  (run-test-commit-rollback)
  (run-test-all-or-nothing)
  (run-test-property-random)
  (format t "All tests passed.~%"))

(main)

