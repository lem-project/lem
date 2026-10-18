;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "registry-tests"
  :license "LicenseRef-Yeditor-Proprietary"
  :depends-on ("yeditor-context" "yeditor-tx" "rove")
  :components ((:file "registry-test"))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :rove '#:run c)))
