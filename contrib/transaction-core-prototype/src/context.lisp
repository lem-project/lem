;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-prototype-context
  (:use :cl)
  (:export #:effect))
(in-package :yeditor-prototype-context)

(defun effect (component kind function &key undo-thunk)
  "Queue COMPONENT's effect in the current transaction, returning its handle.
UNDO-THUNK captures the inverse before installation; no effect runs until commit."
  (yeditor-prototype-tx:install-effect component kind function :undo-thunk undo-thunk))
