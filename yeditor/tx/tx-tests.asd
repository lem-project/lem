;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-tx-tests"
  :license "LicenseRef-Yeditor-Proprietary"
  :description "Tests for yeditor-tx"
  :depends-on ("yeditor-tx" "rove")
  :components ((:file "tx-test") (:file "tx2-test") (:file "side-test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove '#:run :yeditor-tx-tests)))
