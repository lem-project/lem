;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-effects-tests"
  :license "LicenseRef-Yeditor-Proprietary"
  :description "Tests for yeditor-effects"
  :depends-on ("yeditor-effects" "yeditor-tx" "yeditor-context" "rove")
  :components ((:file "effects-tests"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove '#:run :yeditor-effects-tests)))
