;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; From the repository root: sbcl --script scripts/run-yeditor-lsp-tests.lisp
;;; Uses local stdio fixtures only; no Python language server installation needed.
(load ".qlot/setup.lisp")
(ql:quickload '("yeditor-components-tests" "yeditor-effects-t6-tests"))
(lem/common/timer:with-timer-manager
    (make-instance 'yeditor-components/lsp-tests::event-timer-manager)
  (dolist (test '(yeditor-components-tests::test-le-lsp-session-start-stop
                  yeditor-components-tests::test-le-lsp-modeline-segment
                  yeditor-components-tests::test-le-lsp-missing-server-reports-install-command
                  yeditor-components/lsp-tests::python-projects-have-isolated-reversible-servers
                  yeditor-components/lsp-tests::missing-server-offers-guidance-without-starting-a-process
                  yeditor-components/lsp-tests::borrowed-session-detaches-each-acquired-buffer))
    (unless (rove:run-test test) (uiop:quit 1)))
  (unless (rove:run :yeditor-effects-t6-tests) (uiop:quit 1)))
(uiop:quit 0)
