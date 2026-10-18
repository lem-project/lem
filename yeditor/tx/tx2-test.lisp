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
(defvar *undo-order* nil) ; records order of undo execution for directional test

(defun inc-fn () (incf *counter*))
(defun dec-fn () (decf *counter*))

(defun record-undo (id)
  (lambda () (setf *undo-order* (append *undo-order* (list id)))))

(deftest tx2-commit-rollback
  "Test tx-commit and tx-rollback with multiple effects."
  (let ((*counter* 0) (*tx-pending* nil) (*tx-committed* nil))
    (with-transaction
      (dotimes (i 5)
        (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)))
    (tx-commit)
    (ok (= *counter* 5))
    (tx-rollback)
    (ok (= *counter* 0))))

(deftest tx2-all-or-nothing
  "Test that an error aborts all effects."
  (let ((*counter* 0) (*tx-pending* nil) (*tx-committed* nil))
    (with-transaction
      (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
      (install-effect :test :inc #'inc-fn :undo-thunk #'dec-fn)
      (install-effect :test :bad (lambda () (error "boom")) :undo-thunk #'dec-fn))
    (handler-case
        (tx-commit)
      (error (e)
        (declare (ignore e))
        (ok t)))
    (ok (= *counter* 0))))

(deftest tx2-directional-rove
  "Verify tx-rollback runs undo thunks in LIFO order."
  (let ((*counter* 0) (*undo-order* nil) (*tx-pending* nil) (*tx-committed* nil))
    (install-effect :test :a (lambda () nil) :undo-thunk (record-undo :inv1))
    (install-effect :test :b (lambda () nil) :undo-thunk (record-undo :inv2))
    (tx-commit)
    (tx-rollback)
    (ok (equal *undo-order* '(:inv2 :inv1)))))

(deftest tx2-property-random
  "Property‑based test with random effects over 20 seeds, order‑sensitive."
  (dotimes (seed 20)
    (let ((*counter* 0) (*tx-pending* nil) (*tx-committed* nil))
      (sb-ext:seed-random-state (+ 1000 seed))
      (with-transaction
        (dotimes (i 100)
          (let ((choice (random 2)))
            (if (= choice 0)
                (install-effect :test :a (lambda () (setf *counter* (+ (* 2 *counter*) 1)))
                               :undo-thunk (lambda () (setf *counter* (floor (/ (- *counter* 1) 2)))))
                (install-effect :test :b (lambda () (incf *counter*))
                               :undo-thunk (lambda () (decf *counter*)))))))
      (tx-commit)
      (tx-rollback)
      (ok (= *counter* 0)))))

