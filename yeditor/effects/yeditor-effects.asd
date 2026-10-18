;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-effects"
  :license "FSL-1.1-MIT"
  :description "Standard effects T4-T6: buffer, mode, hook, keymap, variable, command, overlay, timer, process, server, lsp, fdefinition"
  :depends-on ("lem/core" "lem/extensions" "yeditor-tx" "yeditor-context")
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
               (:file "e-editor-variable")
               (:file "e-window")
               (:file "e-theme")
               (:file "effects")))