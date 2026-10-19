;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-components-tests"
  :description "T12 tests: zero-chrome, palette live preview, agent HUD"
  :license "FSL-1.1-MIT"
  :depends-on ("lem/core" "lem/extensions" "lem-tree-sitter" "lem-fake-interface" "yeditor-components" "yeditor-context-durability" "usocket" "dexador" "flexi-streams" "rove")
  :components ((:file "components-tests")))
