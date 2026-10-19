;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; Run from the repository root; tests make HTTP requests only to localhost.
(load ".qlot/setup.lisp")
(require :asdf)
(dolist (path '("yeditor/core/tx/yeditor-tx.asd"
                "yeditor/core/context/yeditor-context.asd"
                "yeditor/effects/yeditor-effects.asd"))
  (asdf:load-asd (merge-pathnames path (uiop:getcwd))))
(asdf:load-system "yeditor-components-tests")
(defclass smoke-timer-manager (lem/common/timer:timer-manager) ())
(defmethod lem/common/timer:send-timer-notification ((manager smoke-timer-manager) callback)
  (lem:send-event callback))
(lem/common/timer:with-timer-manager (make-instance 'smoke-timer-manager)
  (dolist (name '("TEST-LE-MCP-WORKER-BUS"
                  "TEST-LE-LSP-SESSION-START-STOP"
                  "TEST-LE-LSP-MISSING-SERVER-REPORTS-INSTALL-COMMAND"
                  "TEST-HUD-MOUNTS-AND-UNMOUNTS-WITH-AGENT-TX"))
    (format t "Running agent smoke test: ~A~%" name)
    (let ((rove:*debug-on-error* t))
      (unless (rove:run-test (find-symbol name :yeditor-components-tests))
        (error "Agent smoke test failed: ~A" name)))))
(uiop:quit 0)
