;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

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
