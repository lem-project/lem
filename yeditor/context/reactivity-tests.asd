;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-context-reactivity-tests"
  :license "LicenseRef-Yeditor-Proprietary"
  :description "T7 tests: coeffect classification, context-event dispatch, work-cycle predicates"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-reactivity" "rove")
  :components ((:file "reactivity-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
