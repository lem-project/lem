;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-prototype-sdk/rove-tests
  (:use :cl :rove))
(in-package :yeditor-prototype-sdk/rove-tests)

(deftest component-lifecycle
  (ok (yeditor-prototype-sdk/tests:run-tests)))
