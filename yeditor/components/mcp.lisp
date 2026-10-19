;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/mcp
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-mcp
           :*mcp-hostname*
           :*mcp-port*
           :*worker-protocol*
           :*host-capabilities*
           :install-mcp-tool
           :register-worker
           :unregister-worker
           :worker-ids
           :submit-task
           :next-task
           :report-task
           :task-observations
           :component-states))
(in-package :yeditor-components/mcp)

;;;; le-mcp (plan T19): the low-latency bus between the editor and agent workers,
;;;; on top of Lem's MCP server (Streamable HTTP at http://HOST:PORT/mcp).
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
     ;; forget workers and tasks when the bus goes away
     (yeditor-tx:install-effect comp :noop (lambda () nil)
                                :undo-thunk (lambda ()
                                              (bt2:with-lock-held (*bus-lock*)
                                                (clrhash *workers*)
                                                (setf *tasks* nil)))))))
