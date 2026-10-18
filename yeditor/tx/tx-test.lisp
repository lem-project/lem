;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

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
