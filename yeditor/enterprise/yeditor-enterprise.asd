;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defsystem "yeditor-enterprise"
  :description "Yeditor Enterprise (L4-L5): journal, agent host, transactional branching, forge"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "LicenseRef-Yeditor-Enterprise"
  :version "0.1.0"
  :depends-on ("yeditor-core" "yeditor-components" "dexador" "yason")
  :components ((:file "agent/workers")
               (:file "forge/forge")))
