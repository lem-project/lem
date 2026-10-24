;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; Local fixtures only: no distribution downloads or external package installs.
(load ".qlot/setup.lisp")
(ql:quickload "yeditor-components-tests")
(lem/common/timer:with-timer-manager
    (make-instance 'yeditor-components/lsp-tests::event-timer-manager)
  (dolist (test '(yeditor-components/packages-tests::install-is-idempotent-and-preserves-source-registration
                  yeditor-components/packages-tests::failed-load-cleans-new-packages-and-registration
                  yeditor-components/packages-tests::scan-the-active-quicklisp-local-projects
                  yeditor-components/packages-tests::palette-install-remove-100-cycles
                  yeditor-components/packages-tests::package-task-delivers-results-on-the-ui-thread
                  yeditor-components/packages-tests::failed-package-task-releases-the-worker-slot
                  yeditor-components/packages-tests::load-and-remove-a-package-through-the-worker
                  yeditor-components-tests::test-package-install-uninstall-cycle
                  yeditor-components/packages-tests::removal-cleans-packages-with-use-relationships
                  yeditor-components-tests::test-package-missing-system))
    (unless (rove:run-test test) (uiop:quit 1))))
(uiop:quit 0)
