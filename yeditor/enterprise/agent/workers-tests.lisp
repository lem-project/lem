;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-tests
  (:use :cl :rove :yeditor-context :yeditor-enterprise/workers))
(in-package :yeditor-enterprise-tests)

(defun test-data (name)
  (namestring (asdf:system-relative-pathname "yeditor-enterprise" (format nil "agent/test-data/~A" name))))

(defun observation-of (task)
  (find task (yeditor-components/mcp:task-observations) :key (function first) :test (function equal)))

(defun wait-for (predicate &key (timeout 10))
  (loop :repeat (* timeout 50) :thereis (funcall predicate) :do (sleep 0.02)))

(defmacro with-global ((var value) &body body)
  "Set the global value of VAR (seen by the dispatcher thread) for BODY."
  (let ((old (gensym)))
    `(let ((,old (symbol-value (quote ,var))))
       (setf (symbol-value (quote ,var)) ,value)
       (unwind-protect (progn ,@body) (setf (symbol-value (quote ,var)) ,old)))))

(deftest test-claude-code-adapter
  (with-global (*claude-code-binary* (test-data "fake-claude"))
    (install-component (quote le-worker-claude-code))
    (unwind-protect
         (let* ((adapter (first *adapters*))
                (worker (adapter-worker-id adapter)))
           (ok (member worker (yeditor-components/mcp:worker-ids) :test (function equal))
               "installing registers a worker on the bus")
           (ok (equal (adapter-ping adapter) "9.9.9 (Claude Code, fake)") "discovery ping answered")
           (let ((task (yeditor-components/mcp:submit-task worker "edit" "rename foo to bar")))
             (ok (wait-for (lambda () (observation-of task))) "task taken and reported")
             (ok (equal (third (observation-of task)) "FAKE: [edit] rename foo to bar mcp=ok turns=20")
                 "prompt, MCP config and step budget passed to the CLI")
             (ok (eq (second (observation-of task)) :done) "status done"))
           (let ((task (yeditor-components/mcp:submit-task worker "edit" "FAIL please")))
             (ok (wait-for (lambda () (observation-of task))) "failing task reported")
             (ok (eq (second (observation-of task)) :failed) "status failed")
             (ok (equal (third (observation-of task)) "boom") "agent error passed on")))
      (uninstall-component (quote le-worker-claude-code)))
    (ok (null (yeditor-components/mcp:worker-ids)) "uninstalling unregisters the worker")
    (ok (null *adapters*) "dispatcher stopped")))

(deftest test-http-worker-adapter
  (let* ((process (uiop:launch-program (list "python3" (test-data "fake-hermes.py")) :output :stream))
         (line (read-line (uiop:process-info-output process)))
         (url (format nil "http://127.0.0.1:~A" (subseq line (1+ (position #\Space line))))))
    (unwind-protect
         (with-global (*hermes-url* url)
           (install-component (quote le-worker-hermes))
           (unwind-protect
                (let* ((adapter (first *adapters*))
                       (worker (adapter-worker-id adapter)))
                  (ok (equal (adapter-ping adapter) "hermes (fake)") "discovery ping answered")
                  (let ((task (yeditor-components/mcp:submit-task worker "review" "module x")))
                    (ok (wait-for (lambda () (observation-of task))) "task reported")
                    (ok (equal (third (observation-of task)) "hermes did module x") "observation from the HTTP worker")))
             (uninstall-component (quote le-worker-hermes)))
           (ok (null (yeditor-components/mcp:worker-ids)) "worker unregistered"))
      (uiop:terminate-process process)
      (uiop:wait-process process))))
