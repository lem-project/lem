;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defsystem "yeditor-enterprise-tests"
  :description "Yeditor Enterprise tests"
  :license "LicenseRef-Yeditor-Enterprise"
  :depends-on ("yeditor-enterprise" "rove")
  :components ((:file "agent/workers-tests")))
