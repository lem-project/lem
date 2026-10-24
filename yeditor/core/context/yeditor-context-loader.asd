;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-context-loader"
  :description "T8 loader: .yeditor.lisp parsing, config reconciliation, HMR watcher"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "FSL-1.1-MIT"
  :version "0.1.0"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-reactivity")
  :components ((:file "loader")))
