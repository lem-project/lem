;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise/agent-tools
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/mcp
                #:install-mcp-tool #:worker-name #:*payload-injector*)
  (:import-from :yeditor-components/hud
                #:*agent-tx* #:*agent-tx-close-functions* #:open-agent-tx #:agent-tx-open-file
                #:agent-tx-replace #:agent-tx-id #:agent-tx-worker #:agent-tx-task
                #:agent-tx-edits #:agent-tx-touched)
  (:import-from :yeditor-enterprise/journal
                #:journal-append #:journal-query #:journal-tail #:journal-since
                #:with-journal-author #:record-terminal-run #:record-agent-event)
  (:export :*agent-root*
           :*run-timeout*
           :*max-read-bytes*
           :agent-tool-refused
           :agent-tool-refused-reason
           :project-root
           :resolve-path
           :workspace-read
           :workspace-list
           :workspace-glob
           :workspace-search
           :workspace-edit
           :terminal-run
           :lsp-diagnostics
           :git-status
           :git-diff
           :journal-events
           :ai-complete
           :agent-transactions
           :bootstrap-payload
           :le-agent-tools-workspace
           :le-agent-tools-terminal
           :le-agent-tools-lsp
           :le-agent-tools-git
           :le-agent-tools-journal
           :le-agent-tools-ai
           :le-agent-host))
(in-package :yeditor-enterprise/agent-tools)

;;;; Plan T21: one tool surface for every agent — the internal one and, through le-mcp,
;;;; external workers (Hermes, Claude Code). Each group is a toggleable component. All
;;;; edits go through editor buffers inside the open agent transaction (each edit a
;;;; workspace side-transaction named <agent-tx-id>/<n>), never straight to disk; on
;;;; commit the touched buffers are saved through the editor and journaled as the agent.
;;;; Paths outside the project root and binary files are refused, and every refusal is
;;;; journaled as an audit event.

(defparameter *agent-root* nil
  "Project root the tools are confined to; nil means the project of the current buffer.")
(defparameter *run-timeout* 120 "Seconds a terminal/run command may take.")
(defparameter *max-read-bytes* (* 2 1024 1024) "Larger files are refused by workspace/read.")
(defparameter *skipped-directories* (list ".git" ".venv" "venv" "node_modules" "__pycache__" ".qlot"))

(define-condition agent-tool-refused (error)
  ((reason :initarg :reason :reader agent-tool-refused-reason))
  (:report (lambda (c s) (format s "Refused: ~A" (agent-tool-refused-reason c)))))

(defun refuse (worker tool path reason)
  "Journal the refused call as an audit event and signal agent-tool-refused."
  (journal-append "audit-event" :tool tool :path (and path (princ-to-string path))
                                :reason reason :author (format nil "agent:~A" worker))
  (error (quote agent-tool-refused) :reason reason))

;;; Sandbox

(defun project-root ()
  (uiop:ensure-directory-pathname
   (or *agent-root*
       (lem-core/commands/project:find-root (buffer-directory (current-buffer)))
       (uiop:getcwd))))

(defun normalize (namestring)
  "NAMESTRING with . and .. segments resolved lexically (no file system access)."
  (let ((out nil))
    (dolist (part (uiop:split-string namestring :separator "/"))
      (cond ((or (string= part "") (string= part ".")))
            ((string= part "..") (pop out))
            (t (push part out))))
    (format nil "/~{~A~^/~}" (reverse out))))

(defun inside-p (path root)
  (let ((path (string-right-trim "/" path))
        (root (string-right-trim "/" root)))
    (or (string= path root)
        (uiop:string-prefix-p (concatenate (quote string) root "/") path))))

(defun resolve-path (path &key (root (project-root)) (worker "external") (tool "workspace"))
  "Absolute namestring of PATH (relative to ROOT); refused when it leaves ROOT, also
   through a symbolic link."
  (let* ((root-name (namestring (or (probe-file root) root)))
         (absolute (normalize (if (uiop:string-prefix-p "/" path)
                                  path
                                  (concatenate (quote string) root-name "/" path)))))
    (unless (inside-p absolute root-name)
      (refuse worker tool path "path is outside the project root"))
    (alexandria:when-let ((real (ignore-errors (probe-file absolute))))
      (unless (inside-p (string-right-trim "/" (namestring real)) root-name)
        (refuse worker tool path "path leads outside the project root through a link")))
    absolute))

(defun binary-file-p (path)
  (with-open-file (in path :element-type (quote (unsigned-byte 8)) :if-does-not-exist nil)
    (and in
         (let* ((buffer (make-array 8000 :element-type (quote (unsigned-byte 8))))
                (n (read-sequence buffer in)))
           (find 0 buffer :end n)))))

(defun relative (path root)
  (enough-namestring path (namestring (or (probe-file root) root))))

(defun skipped-directory-p (directory)
  (member (car (last (pathname-directory directory))) *skipped-directories* :test (function equal)))

(defun project-files (root)
  "Every file under ROOT outside the skipped directories."
  (let ((files nil))
    (uiop:collect-sub*directories
     root (constantly t) (lambda (dir) (not (skipped-directory-p dir)))
     (lambda (dir) (setf files (nconc (uiop:directory-files dir) files))))
    (sort (mapcar (function namestring) files) (function string<))))

;;; workspace/*

(defun full-text (buffer)
  (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))

(defun workspace-read (path &key (worker "external") (root (project-root)))
  "The text of PATH: the editor buffer when it is open (unsaved edits included), else the file."
  (let ((file (resolve-path path :root root :worker worker :tool "workspace/read")))
    (alexandria:if-let ((buffer (get-file-buffer file)))
      (full-text buffer)
      (progn
        (unless (probe-file file) (refuse worker "workspace/read" path "no such file"))
        (when (uiop:directory-pathname-p (probe-file file))
          (refuse worker "workspace/read" path "is a directory"))
        (when (binary-file-p file) (refuse worker "workspace/read" path "binary file"))
        (when (> (with-open-file (in file) (file-length in)) *max-read-bytes*)
          (refuse worker "workspace/read" path "file too large"))
        (uiop:read-file-string file)))))

(defun workspace-list (path &key (worker "external") (root (project-root)))
  "Entries of directory PATH (directories end with /)."
  (let ((dir (uiop:ensure-directory-pathname
              (resolve-path (if (equal path "") "." path) :root root :worker worker :tool "workspace/list"))))
    (unless (uiop:directory-exists-p dir) (refuse worker "workspace/list" path "no such directory"))
    (append (mapcar (lambda (d) (relative d root)) (uiop:subdirectories dir))
            (mapcar (lambda (f) (relative f root)) (uiop:directory-files dir)))))

(defun glob-regex (pattern)
  "A regex for the glob PATTERN: ** any path, * within one segment, ? one character."
  (with-output-to-string (out)
    (write-char #\^ out)
    (loop :with i := 0
          :while (< i (length pattern))
          :do (let ((c (char pattern i)))
                (cond ((and (char= c #\*) (< (1+ i) (length pattern)) (char= (char pattern (1+ i)) #\*))
                       (incf i)
                       ;; **/ also matches no directory at all
                       (if (and (< (1+ i) (length pattern)) (char= (char pattern (1+ i)) #\/))
                           (progn (incf i) (write-string "(?:.*/)?" out))
                           (write-string ".*" out)))
                      ((char= c #\*) (write-string "[^/]*" out))
                      ((char= c #\?) (write-string "[^/]" out))
                      (t (write-string (cl-ppcre:quote-meta-chars (string c)) out)))
                (incf i)))
    (write-char #\$ out)))

(defun workspace-glob (pattern &key (worker "external") (root (project-root)))
  "Project-relative paths matching PATTERN (e.g. **/*.py)."
  (declare (ignore worker))
  (let ((scanner (cl-ppcre:create-scanner (glob-regex pattern))))
    (remove-if-not (lambda (path) (cl-ppcre:scan scanner path))
                   (mapcar (lambda (f) (relative f root)) (project-files root)))))

(defun workspace-search (regex &key (glob "**") (limit 200) (worker "external") (root (project-root)))
  "Lines matching REGEX in files matching GLOB, as \"path:line: text\" (at most LIMIT)."
  (let ((scanner (handler-case (cl-ppcre:create-scanner regex)
                   (error (e) (refuse worker "workspace/search" nil (format nil "bad regex: ~A" e)))))
        (out nil) (n 0))
    (dolist (path (workspace-glob glob :root root) (nreverse out))
      (let ((file (namestring (merge-pathnames path (or (probe-file root) root)))))
        (unless (binary-file-p file)
          (with-open-file (in file :if-does-not-exist nil :external-format (quote (:utf-8 :replacement #\?)))
            (when in
              (loop :for line := (read-line in nil) :for number :from 1
                    :while line
                    :do (when (cl-ppcre:scan scanner line)
                          (push (format nil "~A:~D: ~A" path number line) out)
                          (when (>= (incf n) limit) (return-from workspace-search (nreverse out))))))))))))

(defun ensure-agent-tx (worker task)
  "The open agent transaction, opened for WORKER if none is; refused when another
   worker owns it."
  (cond ((null *agent-tx*) (open-agent-tx worker task))
        ((equal (agent-tx-worker *agent-tx*) worker) *agent-tx*)
        (t (refuse worker "workspace/edit" nil
                   (format nil "agent transaction ~A belongs to ~A" (agent-tx-id *agent-tx*)
                           (agent-tx-worker *agent-tx*))))))

(defun workspace-edit (path old-text new-text &key (worker "external") (task "edit") (root (project-root)))
  "Replace the single occurrence of OLD-TEXT in PATH with NEW-TEXT inside the agent
   transaction (an empty OLD-TEXT on a missing file creates it). Returns the edit name."
  (let ((file (resolve-path path :root root :worker worker :tool "workspace/edit")))
    (when (and (probe-file file) (uiop:directory-pathname-p (probe-file file)))
      (refuse worker "workspace/edit" path "is a directory"))
    (when (and (probe-file file) (binary-file-p file))
      (refuse worker "workspace/edit" path "binary file"))
    (let ((creating (and (string= old-text "") (not (probe-file file)) (not (get-file-buffer file)))))
      (ensure-agent-tx worker task)
      (let* ((buffer (agent-tx-open-file file))
             (text (full-text buffer))
             (start (if creating 0 (search old-text text))))
        (cond ((or (null start) (and (not creating) (string= old-text "")))
               (refuse worker "workspace/edit" path "old text not found"))
              ((and (not creating) (search old-text text :start2 (1+ start)))
               (refuse worker "workspace/edit" path "old text occurs more than once; give more context")))
        (let ((name (agent-tx-replace buffer (1+ start) (+ 1 start (length old-text)) new-text)))
          (record-agent-event worker (agent-tx-task *agent-tx*)
                              :tool-calls (list (format nil "workspace/edit ~A" (relative file root)))
                              :observation name)
          name)))))

;;; Commit: save through the editor, journaled as the agent

(defun save-on-commit (tx outcome)
  "Agent transaction close hook: on commit save the edited file buffers as the agent;
   either way journal the outcome."
  (when (eq outcome :commit)
    (with-journal-author ((format nil "agent:~A" (agent-tx-worker tx)) :tx-id (agent-tx-id tx))
      (dolist (buffer (agent-tx-touched tx))
        (when (and (buffer-filename buffer) (not (deleted-buffer-p buffer)))
          (save-buffer buffer)))))
  (record-agent-event (agent-tx-worker tx) (agent-tx-task tx)
                      :observation (format nil "~A ~(~A~) (~D edits)" (agent-tx-id tx) outcome
                                           (length (agent-tx-edits tx)))))

;;; terminal/run

(defun terminal-run (command &key (worker "external") (root (project-root)) (timeout *run-timeout*))
  "Run COMMAND with sh in ROOT; returns (values exit-code output). Journaled as terminal-run."
  (declare (ignore worker))
  (let* ((process (uiop:launch-program (list "sh" "-c" command)
                                       :directory (namestring (or (probe-file root) root))
                                       :output :stream :error-output :output))
         (output nil)
         (reader (bt2:make-thread (lambda () (setf output (uiop:slurp-stream-string
                                                           (uiop:process-info-output process))))))
         (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop :while (and (uiop:process-alive-p process) (< (get-internal-real-time) deadline))
          :do (sleep 0.02))
    (let ((timed-out (uiop:process-alive-p process)))
      (when timed-out (uiop:terminate-process process :urgent t))
      (let ((code (uiop:wait-process process)))
        (bt2:join-thread reader)
        (let ((output (if timed-out (format nil "~A~%[killed after ~D s]" output timeout) output)))
          (record-terminal-run command root code output)
          (values code output))))))

;;; lsp/diagnostics

(defun lsp-diagnostics (path &key (worker "external") (root (project-root)))
  "Diagnostics the language server reported for PATH (an open buffer)."
  (let* ((file (resolve-path path :root root :worker worker :tool "lsp/diagnostics"))
         (buffer (get-file-buffer file)))
    (if buffer (lem-lsp-mode:buffer-diagnostic-list buffer) nil)))

;;; git/*

(defun git (root &rest args)
  (multiple-value-bind (out err code)
      (uiop:run-program (list* "git" "-C" (namestring (or (probe-file root) root)) args)
                        :output :string :error-output :string :ignore-error-status t)
    (if (zerop code) out (format nil "git failed (~D): ~A" code err))))

(defun git-status (&key (root (project-root))) (git root "status" "--short" "--branch"))
(defun git-diff (&key path (root (project-root)))
  (if path (git root "diff" "--" (namestring path)) (git root "diff")))

;;; journal/*, transactions/list, ai/complete

(defun journal-events (&key type path since (limit 50))
  (cond (since (journal-since since :limit limit))
        ((or type path) (journal-query :type type :path path :limit limit))
        (t (journal-tail limit))))

(defun agent-transactions ()
  "The open agent transaction (plist) and the pending side-transaction names."
  (list :open (and *agent-tx*
                   (list :id (agent-tx-id *agent-tx*) :worker (agent-tx-worker *agent-tx*)
                         :task (agent-tx-task *agent-tx*) :edits (length (agent-tx-edits *agent-tx*))))
        :side (mapcar (function car) yeditor-tx-side:*side-transactions*)))

(defun ai-complete (prompt &key system)
  (let ((provider (or yeditor-components/ai:*default-provider*
                      (error (quote agent-tool-refused) :reason "no AI provider configured"))))
    (yeditor-components/models:provider-generate provider prompt :system system)))

;;; Payload injection (zero copy-paste): tasks start with what just happened

(defun event-line (event)
  (let ((type (gethash "type" event)))
    (cond ((equal type "eval-event")
           (format nil "- eval (~A): ~A~@[ => ~A~]~@[~%  traceback: ~A~]"
                   (gethash "language" event) (gethash "code" event)
                   (gethash "value" event) (gethash "traceback" event)))
          ((equal type "file-edit")
           (format nil "- edit ~A by ~A" (gethash "path" event) (gethash "author" event)))
          ((equal type "terminal-run")
           (format nil "- ran `~A` (exit ~A)" (gethash "command" event) (gethash "exit-code" event)))
          ((equal type "test-result")
           (format nil "- tests ~A: exit ~A~@[, failures ~{~A~^ ~}~]" (gethash "target" event)
                   (gethash "exit-code" event) (gethash "failures" event)))
          (t (format nil "- ~A" type)))))

(defun bootstrap-payload (payload &key (evals 3) (edits 5) (runs 3))
  "PAYLOAD preceded by the recent eval-events, file edits, runs/tests and the
   diagnostics of open buffers, so the worker sees the last input and traceback."
  (let ((events (append (journal-query :type "eval-event" :limit evals)
                        (journal-query :type "file-edit" :limit edits)
                        (journal-query :type "terminal-run" :limit runs)
                        (journal-query :type "test-result" :limit runs)))
        (diagnostics (loop :for buffer :in (buffer-list)
                           :for file := (buffer-filename buffer)
                           :for list := (and file (ignore-errors (lem-lsp-mode:buffer-diagnostic-list buffer)))
                           :when list
                             :collect (format nil "- ~A: ~{~A~^; ~}" file
                                              (mapcar (lambda (d) (format nil "~D: ~A" (getf d :line) (getf d :message)))
                                                      list)))))
    (if (or events diagnostics)
        (format nil "Context from the editor:~%~{~A~%~}~{~A~%~}~%Task: ~A"
                (mapcar (function event-line) events) diagnostics payload)
        payload)))

(defun inject (worker-id kind payload)
  (declare (ignore worker-id kind))
  (if (stringp payload) (bootstrap-payload payload) payload))

;;; MCP tools

(defun arg (arguments name)
  (cdr (assoc name arguments :test (function string=))))

(defun json (object)
  (with-output-to-string (s) (yason:encode object s)))

(defun plist-table (plist)
  (let ((table (make-hash-table :test (function equal))))
    (loop :for (k v) :on plist :by (function cddr)
          :do (setf (gethash (string-downcase (symbol-name k)) table)
                    (if (keywordp v) (string-downcase (symbol-name v)) v)))
    table))

(defun caller (arguments)
  "The calling worker: its registered name when worker_id is given, else external."
  (or (alexandria:when-let ((id (arg arguments "worker_id"))) (worker-name id))
      "external"))

(defun tool-result (thunk)
  "Run THUNK; a refusal becomes {\"error\": reason} instead of an MCP error."
  (handler-case (funcall thunk)
    (agent-tool-refused (e) (json (plist-table (list :error (agent-tool-refused-reason e)))))))

(defun schema (properties &optional required)
  `(("type" . "object")
    ("properties" . ,(or (mapcar (lambda (p) (cons p '(("type" . "string")))) properties)
                         (make-hash-table)))
    ,@(when required `(("required" . ,required)))))

(defmacro define-tool-group (component (&rest tools) &body extra-effects)
  "A component installing TOOLS: each (name description properties required thread-safe
   lambda-list-over-arguments body...)."
  `(define-component ,component
     :version "0.1.0"
     :install (lambda (comp ctx)
                (declare (ignore ctx))
                (list ,@(loop :for (name description properties required thread-safe (args) . body) :in tools
                              :collect `(install-mcp-tool comp ,name ,description
                                                          (schema (list ,@properties) (list ,@required))
                                                          (lambda (,args) (declare (ignorable ,args)) (tool-result (lambda () ,@body)))
                                                          :thread-safe ,thread-safe))
                      ,@extra-effects))))

(define-tool-group le-agent-tools-workspace
    (("yeditor_workspace_read" "Read a project file (the open buffer, with unsaved edits, if any)."
      ("path" "worker_id") ("path") nil (a)
      (workspace-read (arg a "path") :worker (caller a)))
     ("yeditor_workspace_list" "List a project directory (directories end with /)."
      ("path" "worker_id") () nil (a)
      (json (workspace-list (or (arg a "path") "") :worker (caller a))))
     ("yeditor_workspace_glob" "Project-relative paths matching a glob (** any depth, * one segment)."
      ("pattern" "worker_id") ("pattern") nil (a)
      (json (workspace-glob (arg a "pattern") :worker (caller a))))
     ("yeditor_workspace_search" "Lines matching a regex, as path:line: text."
      ("regex" "glob" "worker_id") ("regex") nil (a)
      (json (workspace-search (arg a "regex") :glob (or (arg a "glob") "**") :worker (caller a))))
     ("yeditor_workspace_edit"
      "Replace the single occurrence of old_text with new_text in a project file (empty old_text on a new path creates it). The edit is staged in the editor's agent transaction: the user commits (saves) or rolls it back."
      ("path" "old_text" "new_text" "task" "worker_id") ("path" "old_text" "new_text") nil (a)
      (json (plist-table (list :edit (workspace-edit (arg a "path") (arg a "old_text") (arg a "new_text")
                                                     :worker (caller a) :task (or (arg a "task") "edit"))
                               :transaction (agent-tx-id *agent-tx*))))))
  (install-variable comp (quote *agent-tx-close-functions*)
                    (cons (quote save-on-commit) *agent-tx-close-functions*)))

(define-tool-group le-agent-tools-terminal
    (("yeditor_terminal_run" "Run a shell command in the project root; returns exit code and output."
      ("command" "worker_id") ("command") t (a)
      (multiple-value-bind (code output) (terminal-run (arg a "command") :worker (caller a))
        (json (plist-table (list :exit_code code :output output)))))))

(define-tool-group le-agent-tools-lsp
    (("yeditor_lsp_diagnostics" "Language server diagnostics of an open project file."
      ("path" "worker_id") ("path") nil (a)
      (json (mapcar (function plist-table) (lsp-diagnostics (arg a "path") :worker (caller a)))))))

(define-tool-group le-agent-tools-git
    (("yeditor_git_status" "git status of the project." () () nil (a)
      (git-status))
     ("yeditor_git_diff" "git diff of the project or one path." ("path") () nil (a)
      (git-diff :path (arg a "path")))))

(define-tool-group le-agent-tools-journal
    (("yeditor_journal_query" "Workspace journal events, newest first (type, path, since = universal time, limit)."
      ("type" "path" "since" "limit") () t (a)
      (json (journal-events :type (arg a "type") :path (arg a "path")
                            :since (ignore-errors (parse-integer (arg a "since")))
                            :limit (or (ignore-errors (parse-integer (arg a "limit"))) 50))))
     ("yeditor_agent_transactions" "The open agent transaction and pending side-transactions."
      () () t (a)
      (let ((state (agent-transactions)))
        (json (plist-table (list :open (and (getf state :open) (plist-table (getf state :open)))
                                 :side (getf state :side))))))))

(define-tool-group le-agent-tools-ai
    (("yeditor_ai_complete" "Complete a prompt with the editor's configured model (local by default)."
      ("prompt" "system") ("prompt") t (a)
      (ai-complete (arg a "prompt") :system (arg a "system")))))

;;; The agent host: the whole surface plus payload injection

(defparameter *host-tool-groups*
  (list (quote le-agent-tools-workspace) (quote le-agent-tools-lsp)
        (quote le-agent-tools-git) (quote le-agent-tools-journal))
  "Tool groups le-agent-host installs; terminal and ai stay opt-in.")

(defun component-active-p (name)
  (getf (gethash name *component-registry*) :state))

(define-component le-agent-host
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (let ((installed nil))
               (list (install-variable comp (quote *payload-injector*) (quote inject))
                     (yeditor-tx:install-effect
                      comp :components
                      (lambda ()
                        (dolist (name *host-tool-groups*)
                          (unless (component-active-p name)
                            (install-component name)
                            (push name installed))))
                      :undo-thunk
                      (lambda ()
                        ;; only the groups this host installed, newest first
                        (dolist (name installed)
                          (uninstall-component name))
                        (setf installed nil)))))))
