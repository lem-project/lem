;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-effects"
  :license "LicenseRef-Yeditor-Proprietary"
  :description "Standard effects T4-T6: buffer, mode, hook, keymap, variable, command, overlay, timer, process, server, lsp, fdefinition"
  :depends-on ("lem" "yeditor-tx" "yeditor-context")
  :components ((:file "e-buffer")
               (:file "e-mode")
               (:file "e-hook")
               (:file "e-keymap")
               (:file "e-variable")
               (:file "e-command")
               (:file "e-overlay")
               (:file "e-timer")
               (:file "e-process")
               (:file "e-server")
               (:file "e-lsp")
               (:file "e-fdefinition")
               (:file "effects")))