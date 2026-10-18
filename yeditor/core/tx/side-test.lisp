;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :lem-tests/side-test
  (:use :cl :rove)
  (:import-from :yeditor-tx-side
    #:side-transaction
    #:side-undo
    #:side-clear
    #:*side-transactions*))
(in-package :lem-tests/side-test)

;; Global state for testing side effects
(defvar *side-state* 0)
;; Count of warnings signalled during a test
(defvar *warn-count* 0)

(deftest side-transaction-runs-thunk
  "Define a side transaction and verify the thunk runs and state changes."
  (let ((*side-state* 0)
        (*side-transactions* nil))
    (side-transaction "inc" (lambda () (incf *side-state*)) (lambda () (decf *side-state*)))
    (ok (= *side-state* 1))))

(deftest side-undo-reverts-and-clears
  "side-undo should invoke undo thunk and clear the registry."
  (let ((*side-state* 0)
        (*side-transactions* nil))
    (side-transaction "inc" (lambda () (incf *side-state*)) (lambda () (decf *side-state*)))
    (side-undo "inc")
    (ok (= *side-state* 0))
    (ok (null *side-transactions*))))

(deftest side-redefine-warns-and-replaces
  "Redefining the same name warns, undoes the OLD effect, installs the NEW effect."
  (let ((*side-state* 0)
        (*warn-count* 0)
        (*side-transactions* nil))
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (incf *warn-count*))))
      (side-transaction "inc" (lambda () (incf *side-state*)) (lambda () (decf *side-state*)))
      (side-transaction "inc" (lambda () (incf *side-state*)) (lambda () (decf *side-state* 2)))
      (ok (= *warn-count* 1))
      (ok (= *side-state* 1))
      (side-undo "inc")
      (ok (= *side-state* -1))
      (ok (null *side-transactions*)))))

(deftest side-lifo-multiple-transactions
  "Multiple side transactions should be undoable in any order."
  (let ((*side-state* 0)
        (*side-transactions* nil))
    (side-transaction "first" (lambda () (incf *side-state*)) (lambda () (decf *side-state*)))
    (side-transaction "second" (lambda () (incf *side-state*)) (lambda () (decf *side-state*)))
    (ok (= *side-state* 2))
    (side-undo "second")
    (ok (= *side-state* 1))
    (side-undo "first")
    (ok (= *side-state* 0))
    (ok (null *side-transactions*))))