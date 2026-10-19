;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-tests/agent-tools
  (:use :cl :rove :yeditor-context :yeditor-enterprise/agent-tools)
  (:import-from :yeditor-enterprise/journal
                #:*journal-path* #:journal-query #:record-eval #:le-journal)
  (:import-from :yeditor-components/hud
                #:*agent-tx* #:close-agent-tx #:agent-tx-id))
(in-package :yeditor-enterprise-tests/agent-tools)

(defun write-test-file (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out)))

(defun file-text (path)
  (uiop:read-file-string path))

(defun call-with-project (function)
  "Run FUNCTION with a fresh project (src/a.py, src/b.py, README.md, a binary file) as
   the agent root and a journal of its own."
  (let* ((yeditor-components/mcp:*mcp-port* yeditor-components/mcp:*mcp-port*)
         (root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "yeditor-agent-~D/" (random 1000000))
                                 (uiop:default-temporary-directory))))
         (*agent-root* root)
         (*journal-path* (merge-pathnames "journal/journal.jsonl" root)))
    (write-test-file (merge-pathnames "src/a.py" root) (format nil "def f():~%    return 1~%"))
    (write-test-file (merge-pathnames "src/b.py" root) (format nil "import a~%print(a.f())~%"))
    (write-test-file (merge-pathnames "README.md" root) (format nil "# demo~%"))
    (with-open-file (out (merge-pathnames "data.bin" root) :direction :output
                                                           :element-type (quote (unsigned-byte 8)))
      (write-sequence (make-array 16 :element-type (quote (unsigned-byte 8)) :initial-element 0) out))
    (lem-fake-interface:with-fake-interface ()
      (lem/common/timer:with-timer-manager (make-instance (quote lem/common/timer:timer-manager))
        (install-component (quote le-journal))
        (unwind-protect (funcall function root)
          (when *agent-tx* (ignore-errors (close-agent-tx :rollback t)))
          (uninstall-component (quote le-journal))
          (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))

(defmacro with-project ((root) &body body)
  `(call-with-project (lambda (,root) (declare (ignorable ,root)) ,@body)))

(defun free-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (prog1 (usocket:get-local-port socket) (usocket:socket-close socket))))

(defun refused-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (agent-tool-refused () t)))

(defun audits ()
  (journal-query :type "audit-event"))

(deftest test-sandbox-refuses-and-audits
  (with-project (root)
    (ok (refused-p (lambda () (workspace-read "../outside.txt" :worker "w"))) "relative escape refused")
    (ok (refused-p (lambda () (workspace-read "/etc/hostname" :worker "w"))) "absolute path outside refused")
    (ok (refused-p (lambda () (workspace-read "src/../../x" :worker "w"))) ".. inside the path refused")
    (sb-posix:symlink "/etc" (namestring (merge-pathnames "etc-link" root)))
    (ok (refused-p (lambda () (workspace-read "etc-link/hostname" :worker "w"))) "symlink escape refused")
    (ok (refused-p (lambda () (workspace-read "data.bin" :worker "w"))) "binary file refused")
    (ok (refused-p (lambda () (workspace-edit "/tmp/evil.py" "" "x" :worker "w"))) "edit outside refused")
    (ok (= 6 (length (audits))) "every refusal is an audit event")
    (ok (equal "agent:w" (gethash "author" (first (audits)))) "audit names the worker")))

(deftest test-workspace-read-list-glob-search
  (with-project (root)
    (ok (search "return 1" (workspace-read "src/a.py")) "read from disk")
    (ok (equal (list "src/") (remove-if-not (lambda (e) (equal e "src/")) (workspace-list ""))) "list root")
    (ok (equal (list "src/a.py" "src/b.py") (workspace-list "src")) "list directory")
    (ok (equal (list "src/a.py" "src/b.py") (workspace-glob "**/*.py")) "glob any depth")
    (ok (equal (list "README.md") (workspace-glob "*.md")) "glob one segment")
    (ok (equal (list "src/b.py:2: print(a.f())") (workspace-search "a\\.f\\(" :glob "**/*.py")) "search")
    (let ((buffer (lem:find-file-buffer (merge-pathnames "src/a.py" root))))
      (lem:insert-string (lem:buffer-start-point buffer) "# unsaved")
      (ok (uiop:string-prefix-p "# unsaved" (workspace-read "src/a.py")) "read sees the open buffer")
      (lem:buffer-unmark buffer)
      (lem:delete-buffer buffer))))

(deftest test-workspace-edit-rollback-leaves-disk-identical
  (with-project (root)
    (let ((a (merge-pathnames "src/a.py" root))
          (before (file-text (merge-pathnames "src/a.py" root))))
      (workspace-edit "src/a.py" "return 1" "return 2" :worker "hermes" :task "bump")
      (workspace-edit "src/new.py" "" (format nil "x = 1~%") :worker "hermes")
      (let ((id (agent-tx-id *agent-tx*)))
        (ok (search "return 2" (workspace-read "src/a.py")) "edit applied in the buffer")
        (ok (equal before (file-text a)) "nothing written to disk before commit")
        (ok (not (probe-file (merge-pathnames "src/new.py" root))) "new file not on disk before commit")
        (ok (= 2 (count-if (lambda (name) (uiop:string-prefix-p (format nil "~A/" id) name))
                           (mapcar (function car) yeditor-tx-side:*side-transactions*)))
            "each edit is a side-transaction named after the agent-tx-id")
        (ok (refused-p (lambda () (workspace-edit "src/b.py" "import a" "import b" :worker "claude")))
            "another worker cannot edit inside this transaction")
        (close-agent-tx :rollback t)
        (ok (equal before (file-text a)) "disk byte-identical after rollback")
        (ok (search "return 1" (workspace-read "src/a.py")) "buffer restored")
        (ok (null (lem:get-file-buffer (merge-pathnames "src/new.py" root))) "buffer opened by the agent closed")
        (ok (notany (lambda (name) (uiop:string-prefix-p id name))
                    (mapcar (function car) yeditor-tx-side:*side-transactions*))
            "no side-transaction left")))))

(deftest test-workspace-edit-commit-saves-as-agent
  (with-project (root)
    (install-component (quote le-agent-tools-workspace))
    (unwind-protect
         (let ((a (namestring (merge-pathnames "src/a.py" root))))
           (ok (refused-p (lambda () (workspace-edit "src/a.py" "nope" "x" :worker "hermes")))
               "missing old text refused")
           (workspace-edit "src/a.py" "return 1" "return 2" :worker "hermes")
           (let ((id (agent-tx-id *agent-tx*)))
             (ok (refused-p (lambda () (workspace-edit "src/a.py" "e" "E" :worker "hermes")))
                 "ambiguous old text refused")
             (close-agent-tx)
             (ok (search "return 2" (file-text a)) "commit saves through the editor")
             (let ((edit (first (journal-query :type "file-edit" :path a))))
               (ok edit "the save is journaled")
               (ok (equal "agent:hermes" (gethash "author" edit)) "as the agent")
               (ok (equal id (gethash "tx_id" edit)) "with the agent-tx-id"))))
      (uninstall-component (quote le-agent-tools-workspace)))
    (ok (null yeditor-components/hud:*agent-tx-close-functions*) "close hook removed on uninstall")))

(deftest test-terminal-run-git-and-journal
  (with-project (root)
    (multiple-value-bind (code output) (terminal-run "echo hello; exit 3")
      (ok (= 3 code) "exit code")
      (ok (search "hello" output) "output"))
    (ok (equal "echo hello; exit 3" (gethash "command" (first (journal-query :type "terminal-run"))))
        "run journaled")
    (multiple-value-bind (code output) (terminal-run "sleep 5" :timeout 1)
      (declare (ignore code))
      (ok (search "killed after 1 s" output) "timeout kills the command"))
    (terminal-run "git init -q . && git add src && git -c user.email=t@t -c user.name=t commit -qm init")
    (write-test-file (merge-pathnames "src/a.py" root) "changed")
    (ok (search "M src/a.py" (git-status)) "git status")
    (ok (search "+changed" (git-diff :path "src/a.py")) "git diff")
    (ok (journal-events :type "terminal-run") "journal query")))

(deftest test-payload-injection-and-host
  (with-project (root)
    (record-eval (list :code "1/0" :language "python" :traceback "ZeroDivisionError: division by zero"))
    (let ((payload (bootstrap-payload "fix the bug")))
      (ok (search "ZeroDivisionError" payload) "last traceback injected")
      (ok (search "Task: fix the bug" payload) "task kept"))
    (setf yeditor-components/mcp:*mcp-port* (free-port))
    (install-component (quote yeditor-components:le-mcp))
    (install-component (quote le-agent-host))
    (unwind-protect
         (let* ((worker (yeditor-components/mcp:register-worker "w" "yeditor-worker/1" (list "tasks"))))
           (yeditor-components/mcp:submit-task worker "fix" "fix the bug")
           (let ((task (yeditor-components/mcp:next-task worker)))
             (ok (search "ZeroDivisionError" (getf task :payload)) "bus tasks are bootstrapped"))
           (ok (gethash "yeditor_workspace_edit" lem-mcp-server::*registered-tools*) "workspace tools on the bus")
           (ok (null (gethash "yeditor_terminal_run" lem-mcp-server::*registered-tools*)) "terminal stays opt-in"))
      (uninstall-component (quote le-agent-host))
      (uninstall-component (quote yeditor-components:le-mcp)))
    (ok (null yeditor-components/mcp:*payload-injector*) "injector removed")
    (ok (null (gethash "yeditor_workspace_edit" lem-mcp-server::*registered-tools*)) "tools removed")))
