;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-sdk/rove-tests
  (:use :cl :rove))
(in-package :yeditor-sdk/rove-tests)

(deftest component-lifecycle
  (ok (yeditor-sdk/tests:run-tests)))
