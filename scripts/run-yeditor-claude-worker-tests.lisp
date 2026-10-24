;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).
(load ".qlot/setup.lisp")
(ql:quickload "yeditor-enterprise-tests")
(dolist (test '(yeditor-enterprise-tests::test-claude-code-adapter
                yeditor-enterprise-tests::subprocess-cancellation-preempts-the-task-timeout
                yeditor-enterprise-tests::claude-adapter-100-registration-cycles
                yeditor-enterprise-tests::removing-claude-worker-cancels-an-active-cli))
  (unless (rove:run-test test) (uiop:quit 1)))
(uiop:quit 0)
