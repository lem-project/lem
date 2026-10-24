;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.
(load ".qlot/setup.lisp")
(ql:quickload "yeditor-components-tests")
(lem/common/timer:with-timer-manager
    (make-instance 'yeditor-components/lsp-tests::event-timer-manager)
 (dolist (test '(yeditor-components/models-policy-tests::remote-metadata-and-downloads-require-opt-in
                yeditor-components/models-policy-tests::ai-palette-restores-its-source-100-times
                yeditor-components-tests::test-ai-edits-are-reversible
                yeditor-components-tests::test-ai-components))
  (unless (rove:run-test test) (uiop:quit 1))))
(uiop:quit 0)
