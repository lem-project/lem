;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;;; yeditor.asd — Yeditor: a spatiotemporally composable, agentic code editor.
;;;; Built on Lem (https://github.com/lem-project/lem, MIT) — see THIRD_PARTY_NOTICES.md.
;;;; The open-core build depends only on yeditor-core; the enterprise suite is loaded separately.
(asdf:defsystem #:yeditor
  :description "Yeditor: a spatiotemporally composable, agentic code editor (Lem fork)"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "FSL-1.1-MIT"
  :version "0.1.0"
  :depends-on (:lem :yeditor-core))
