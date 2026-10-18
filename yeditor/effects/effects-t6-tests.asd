;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-effects-t6-tests"
  :license "LicenseRef-Yeditor-Proprietary"
  :description "T6 tests: process, fdefinition effects (integration)"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-effects" "rove")
  :components ((:file "effects-t6-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
