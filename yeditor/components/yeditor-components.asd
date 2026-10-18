;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-components"
  :description "T12 ephemeral UI: zero-chrome basal state, palette live preview, agent HUD"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "FSL-1.1-MIT"
  :version "0.1.0"
  :depends-on ("lem/core" "lem/extensions" "lem-tree-sitter" "lem-process" "lem-terminal" "cl-ppcre" "dexador" "flexi-streams" "quri" "yason" "lem-mcp-server" "yeditor-core")
  :components ((:file "context-bridge")
               (:file "zero-chrome")
               (:file "palette")
               (:file "theme-policy")
               (:file "themes-ergonomic")
               (:file "hud")
               (:file "lsp")
               (:file "packages")
               (:file "features")
               (:file "sidebar")
               (:file "terminal")
               (:file "workspace-layout")
               (:file "treesitter")
               (:file "lang-python")
               (:file "models")
               (:file "ai")
               (:file "mcp")
               (:file "components")))
