;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "loader-tests"
  :license "LicenseRef-Yeditor-Proprietary"
  :description "T8 tests: config loader, reconciliation, delta application"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-loader" "rove")
  :components ((:file "loader-tests")))
