;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-context
  (:use :cl)
  (:export #:effect))
(in-package :yeditor-context)

(defun effect (component kind function &key undo-thunk)
  "Queue COMPONENT's effect in the current transaction, returning its handle.
UNDO-THUNK captures the inverse before installation; no effect runs until commit."
  (yeditor-tx:install-effect component kind function :undo-thunk undo-thunk))
