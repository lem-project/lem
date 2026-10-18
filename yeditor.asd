;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

;;;; yeditor.asd — Yeditor: a spatiotemporally composable, agentic code editor.
;;;; Built on Lem (https://github.com/lem-project/lem, MIT) — see THIRD_PARTY_NOTICES.md.
(asdf:defsystem #:yeditor
  :description "Yeditor: a spatiotemporally composable, agentic code editor (Lem fork)"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "LicenseRef-Yeditor-Proprietary"
  :version "0.1.0"
  :depends-on (:lem)
  :components ((:module "yeditor"
                :components ((:file "package")
                             (:file "core")))
               (:module "yeditor/tx"
                :components ((:file "tx")
                             (:file "context")))))