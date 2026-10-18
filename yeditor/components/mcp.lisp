;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/mcp
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/lang-python
                #:python-eval #:python-test-failure-attribute)
  (:import-from :yeditor-components/hud
                #:open-agent-tx)
  (:export :le-mcp
           :*mcp-hostname*
           :*mcp-port*
           :*worker-protocol*
           :*host-capabilities*
           :install-mcp-tool
           :register-worker
           :unregister-worker
           :worker-ids
           :worker-name
           :submit-task
           :next-task
           :report-task
           :task-observations
           :component-states))
(in-package :yeditor-components/mcp)

;;;; Editor control tools (Director Mode, plan T21): the agent operates the Lem
;;;; UI in the user's behalf — run any command, present files side by side, eval
;;;; code and annotate the result, and open an agent transaction (checkpoint)
;;;; the user approves with one key. All effects flow through yeditor-tx so even
;;;; window layouts stay reversible.

(defun editor-result (&rest keys-and-values)
  (json-string (apply (function alist) keys-and-values)))

(defun editor-error-string (condition)
  (format nil "~A" condition))

(defun run-lem-command (name)
  "Run the command registered under NAME; returns (values ok error-string)."
  (handler-case
      (progn (call-command (find-command name) nil)
             (values t nil))
    (error (e) (values nil (editor-error-string e)))))

(defun main-window-p (window)
  (not (typep window (quote lem-core:side-window))))

(defun buffer-by-name (name)
  (alexandria:when-let ((buffer (get-buffer name)))
    buffer))

(defun present-single (paths)
  (let ((last nil))
    (dolist (path paths)
      (setf last (find-file-buffer path)))
    (when last
      (switch-to-buffer last))
    last))

(defun present-side-by-side (paths)
  "Split the focused main window and lay PATHS out left to right; returns the
last buffer presented."
  (let ((last nil))
    (dolist (path paths)
      (let ((buffer (find-file-buffer path)))
        (cond (last (split-active-window-vertically)
                    (switch-to-buffer buffer))
              (t (switch-to-buffer buffer)))
        (setf last buffer)))
    last))

(defun annotate-buffer (buffer line text attribute)
  "Put TEXT as a ghost overlay after line LINE of BUFFER."
  (with-point ((s (buffer-start-point buffer))
               (e (buffer-start-point buffer)))
    (move-to-line s line)
    (move-to-line e line)
    (line-end e)
    (insert-string e (format nil "  <- ~A" text) :attribute attribute)
    (make-overlay e s (quote modeline-name))))
;;;; Tools for component management (list, status, toggle, transaction log) and a
;;;; worker protocol: register (handshake + capability check), unregister, take the
;;;; next task, report an observation. Tasks and observations are also pushed to
;;;; every connected client as JSON-RPC notifications on the SSE stream.

(defparameter *mcp-hostname* "127.0.0.1")
(defparameter *mcp-port* 7890)
(defparameter *worker-protocol* "yeditor-worker/1")
(defparameter *host-capabilities* (list "tasks" "events" "observations")
  "Features the host offers; a worker requiring anything else is refused.")

(defvar *bus-lock* (bt2:make-lock :name "yeditor-mcp-bus"))
(defvar *workers* (make-hash-table :test (function equal)) "worker-id -> plist :name :capabilities")
(defvar *tasks* nil "Task plists, oldest first: :id :worker :kind :payload :status :observation")
(defvar *counter* 0)

(defun next-id (prefix)
  (format nil "~A-~D" prefix (incf *counter*)))

(defun publish (method params)
  (alexandria:when-let ((server (lem-mcp-server:current-mcp-server)))
    (lem-mcp-server:publish-event server method params)))

(defun json-string (object)
  (with-output-to-string (s) (yason:encode object s)))

(defun alist (&rest keys-and-values)
  (let ((table (make-hash-table :test (function equal))))
    (loop :for (k v) :on keys-and-values :by (function cddr) :do (setf (gethash k table) v))
    table))

(defun arg (arguments name)
  (cdr (assoc name arguments :test (function string=))))

;;; Worker bus (thread-safe; callable from Lisp or through MCP tools)

(defun register-worker (name protocol capabilities)
  "Handshake: accept a worker speaking *worker-protocol* whose required CAPABILITIES are
   all offered by the host. Returns the new worker id; signals an error otherwise."
  (unless (equal protocol *worker-protocol*)
    (error "Unsupported worker protocol ~S (host speaks ~S)" protocol *worker-protocol*))
  (let ((missing (set-difference capabilities *host-capabilities* :test (function equal))))
    (when missing
      (error "Host does not offer capabilities: ~{~A~^, ~}" missing)))
  (let ((id (bt2:with-lock-held (*bus-lock*)
              (let ((id (next-id "worker")))
                (setf (gethash id *workers*) (list :name name :capabilities capabilities))
                id))))
    (publish "notifications/yeditor/worker" `(("event" . "registered") ("worker_id" . ,id) ("name" . ,name)))
    id))

(defun unregister-worker (worker-id)
  (bt2:with-lock-held (*bus-lock*)
    (remhash worker-id *workers*)
    (setf *tasks* (remove worker-id *tasks* :key (lambda (task) (getf task :worker)) :test (function equal)))))

(defun worker-ids ()
  (bt2:with-lock-held (*bus-lock*) (alexandria:hash-table-keys *workers*)))

(defun ensure-worker (worker-id)
  (unless (bt2:with-lock-held (*bus-lock*) (gethash worker-id *workers*))
    (error "Unknown worker ~S" worker-id)))

(defun submit-task (worker-id kind payload)
  "Queue a task for WORKER-ID and push a notification to the SSE streams. Returns its id."
  (ensure-worker worker-id)
  (let ((id (bt2:with-lock-held (*bus-lock*)
              (let ((id (next-id "task")))
                (setf *tasks* (append *tasks* (list (list :id id :worker worker-id :kind kind
                                                          :payload payload :status :pending
                                                          ;; every key present: setf getf then edits in place
                                                          :observation nil))))
                id))))
    (publish "notifications/yeditor/task"
             `(("worker_id" . ,worker-id) ("task_id" . ,id) ("kind" . ,kind) ("payload" . ,payload)))
    id))

(defun next-task (worker-id)
  "The oldest pending task of WORKER-ID (marked delivered), or nil."
  (ensure-worker worker-id)
  (bt2:with-lock-held (*bus-lock*)
    (let ((task (find-if (lambda (task) (and (equal (getf task :worker) worker-id)
                                             (eq (getf task :status) :pending)))
                         *tasks*)))
      (when task (setf (getf task :status) :delivered))
      task)))

(defun report-task (worker-id task-id status observation)
  (ensure-worker worker-id)
  (bt2:with-lock-held (*bus-lock*)
    (let ((task (find task-id *tasks* :key (lambda (task) (getf task :id)) :test (function equal))))
      (unless task (error "Unknown task ~S" task-id))
      (setf (getf task :status) (intern (string-upcase status) :keyword)
            (getf task :observation) observation)))
  (publish "notifications/yeditor/observation"
           `(("worker_id" . ,worker-id) ("task_id" . ,task-id) ("status" . ,status)))
  t)

(defun task-observations ()
  (bt2:with-lock-held (*bus-lock*)
    (loop :for task :in *tasks* :when (getf task :observation)
          :collect (list (getf task :id) (getf task :status) (getf task :observation)))))

;;; Component management

(defun worker-name (worker-id)
  "The registered name of WORKER-ID, or nil."
  (getf (gethash worker-id *workers*) :name))

(defun component-states ()
  "Alist (name . state) for every registered component."
  (let ((states nil))
    (maphash (lambda (name entry)
               (push (cons (string-downcase (symbol-name name))
                           (cond ((getf entry :state) "active")
                                 ((getf entry :suspended) "suspended")
                                 (t "registered")))
                     states))
             *component-registry*)
    (sort states (function string<) :key (function car))))

(defun find-component (name)
  (or (loop :for symbol :being :the :hash-keys :of *component-registry*
            :when (string-equal (symbol-name symbol) name) :return symbol)
      (error "No component named ~S" name)))

(defun transaction-log (limit)
  (let ((path (or yeditor-context/durability:*effects-log-path*
                  (yeditor-context/durability:default-effects-log-path))))
    (if (probe-file path)
        (let ((lines (uiop:read-file-lines path)))
          (format nil "~{~A~%~}" (last lines (or limit 50))))
        "")))

;;; Tools as reversible effects

(defun install-mcp-tool (comp name description input-schema handler &key thread-safe)
  "Register an MCP tool as a transaction effect; undo removes it."
  (yeditor-tx:install-effect comp :mcp-tool
                             (lambda ()
                               (lem-mcp-server:register-tool name description input-schema handler
                                                             :thread-safe thread-safe))
                             :undo-thunk (lambda () (lem-mcp-server:unregister-tool name))))

(defun schema (&rest properties-and-required)
  "JSON Schema alist for an object with string PROPERTIES (list of names) and REQUIRED."
  (destructuring-bind (&optional properties required) properties-and-required
    `(("type" . "object")
      ("properties" . ,(or (mapcar (lambda (p) (cons p '(("type" . "string")))) properties)
                           (make-hash-table)))
      ,@(when required `(("required" . ,required))))))

(defun install-server (comp)
  "Start Lem's MCP server unless one is already running; undo stops only a server it started."
  (let ((started nil))
    (yeditor-tx:install-effect comp :server
                               (lambda ()
                                 (unless (lem-mcp-server:current-mcp-server)
                                   (lem-mcp-server:mcp-server-start *mcp-hostname* *mcp-port*)
                                   (unless (lem-mcp-server:current-mcp-server)
                                     (error "Could not start the MCP server on ~A:~A" *mcp-hostname* *mcp-port*))
                                   (setf started t)))
                               :undo-thunk
                               (lambda ()
                                 (when started
                                   (lem-mcp-server:stop-mcp-server (lem-mcp-server:current-mcp-server))
                                   (setf started nil))))))

(define-component le-mcp
  :version "0.1.0"
  :install
  (lambda (comp ctx)
    (declare (ignore ctx))
    (list
     (install-server comp)
     (install-mcp-tool comp "yeditor_list_components" "List Yeditor components and their state."
                       (schema)
                       (lambda (args) (declare (ignore args))
                         (json-string (mapcar (lambda (c) (alist "name" (car c) "state" (cdr c)))
                                              (component-states))))
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_component_status" "State of one Yeditor component."
                       (schema (list "name") (list "name"))
                       (lambda (args)
                         (let ((name (arg args "name")))
                           (find-component name)
                           (cdr (assoc name (component-states) :test (function string-equal)))))
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_toggle_component" "Suspend or resume a Yeditor component."
                       (schema (list "name") (list "name"))
                       (lambda (args)
                         (let ((symbol (find-component (arg args "name"))))
                           (toggle-component symbol)
                           (cdr (assoc (arg args "name") (component-states) :test (function string-equal))))))
     (install-mcp-tool comp "yeditor_transaction_log" "Last entries of the effects transaction log (JSONL)."
                       (schema (list "limit"))
                       (lambda (args)
                         (transaction-log (let ((limit (arg args "limit")))
                                            (if (stringp limit) (parse-integer limit :junk-allowed t) limit))))
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_worker_register"
                       "Register an agent worker (handshake). Arguments: name, protocol (yeditor-worker/1), capabilities (array of required host features)."
                       `(("type" . "object")
                         ("properties" . (("name" . (("type" . "string")))
                                          ("protocol" . (("type" . "string")))
                                          ("capabilities" . (("type" . "array") ("items" . (("type" . "string")))))))
                         ("required" . ("name" "protocol")))
                       (lambda (args)
                         (register-worker (arg args "name") (arg args "protocol") (arg args "capabilities")))
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_worker_unregister" "Unregister an agent worker."
                       (schema (list "worker_id") (list "worker_id"))
                       (lambda (args) (unregister-worker (arg args "worker_id")) "ok")
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_task_next" "Take the next pending task of a worker (JSON, or null)."
                       (schema (list "worker_id") (list "worker_id"))
                       (lambda (args)
                         (let ((task (next-task (arg args "worker_id"))))
                           (if task
                               (json-string (alist "task_id" (getf task :id) "kind" (getf task :kind)
                                                   "payload" (getf task :payload)))
                               "null")))
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_task_report" "Report the outcome (status, observation) of a task."
                       (schema (list "worker_id" "task_id" "status" "observation")
                               (list "worker_id" "task_id" "status"))
                       (lambda (args)
                         (report-task (arg args "worker_id") (arg args "task_id")
                                      (arg args "status") (arg args "observation"))
                         "ok")
                       :thread-safe t)
     (install-mcp-tool comp "yeditor_editor_run_command"
                       "Director Mode: run any Lem command by name on the user's behalf."
                       (schema (list "command_name") (list "command_name"))
                       (lambda (args)
                         (multiple-value-bind (ok err) (run-lem-command (arg args "command_name"))
                           (if ok (editor-result "ok" t) (editor-result "ok" nil "error" err)))))
     (install-mcp-tool comp "yeditor_editor_open_and_present"
                       "Director Mode: open files in buffers and lay them out (layout: single | side-by-side)."
                       `(("type" . "object")
                         ("properties" . (("paths" . (("type" . "array") ("items" . (("type" . "string")))))
                                          ("layout" . (("type" . "string") ("enum" . ("single" "side-by-side"))))))
                         ("required" . ("paths")))
                       (lambda (args)
                         (let* ((paths (arg args "paths"))
                                (layout (or (arg args "layout") "single"))
                                (last (if (string= layout "side-by-side")
                                          (present-side-by-side paths)
                                          (present-single paths))))
                           (editor-result "ok" (and last t) "buffer" (and last (buffer-name last)))))
                       :thread-safe nil)
     (install-mcp-tool comp "yeditor_editor_eval_and_annotate"
                       "Director Mode: eval Python code in the REPL, annotate the buffer line and journal it."
                       (schema (list "buffer" "start_line" "end_line" "code"))
                       (lambda (args)
                         (let* ((buffer (buffer-by-name (arg args "buffer")))
                                (start (parse-integer (arg args "start_line")))
                                (end (or (ignore-errors (parse-integer (arg args "end_line"))) start))
                                (code (or (arg args "code")
                                          (and buffer
                                               (with-point ((s (buffer-start-point buffer))
                                                            (e (buffer-end-point buffer)))
                                                 (move-to-line s start)
                                                 (move-to-line e end)
                                                 (line-end e)
                                                 (points-to-string s e)))))
                                (attribute (quote python-test-failure-attribute)))
                           (cond ((and buffer code)
                                  (python-eval code)
                                  (annotate-buffer buffer end "evaluating..." attribute)
                                  (editor-result "ok" t "code" code))
                                 (t (editor-result "ok" nil "error" "buffer not found")))))
                       :thread-safe nil)
     (install-mcp-tool comp "yeditor_editor_propose_checkpoint"
                       "Director Mode: open an agent transaction checkpoint; the user approves (C-c C-a) or reverts (C-c C-k)."
                       (schema (list "worker" "task") (list "worker"))
                       (lambda (args)
                         (open-agent-tx (or (arg args "worker") "hermes")
                                        (or (arg args "task") "checkpoint"))
                         (editor-result "ok" t "coeffect" ":agent-tx-active"))
                       :thread-safe nil)
     (yeditor-tx:install-effect comp :noop (lambda () nil)
                                :undo-thunk (lambda ()
                                              (bt2:with-lock-held (*bus-lock*)
                                                (clrhash *workers*)
                                                (setf *tasks* nil)))))))
