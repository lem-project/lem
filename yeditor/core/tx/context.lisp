;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context-api
  (:use :cl)
  (:import-from :yeditor-tx
   #:install-effect)
  (:export #:effect))
(in-package :yeditor-context-api)

(defun effect (comp kind fn &key undo-thunk)
  "Thin L2 wrapper over yeditor-tx:install-effect (plan Cap. 5.1 context:effect).
   COMP is the owning component symbol, KIND is the effect kind keyword,
   FN is the effect thunk executed at tx-commit."
  (install-effect comp kind fn :undo-thunk undo-thunk))
