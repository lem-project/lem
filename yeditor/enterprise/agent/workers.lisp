;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise/workers
  (:use :cl :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/mcp
                #:register-worker #:unregister-worker #:next-task #:report-task
                #:*worker-protocol* #:*mcp-hostname* #:*mcp-port*)
  (:export :worker-adapter
           :adapter-name
           :adapter-worker-id
           :adapter-ping
           :adapter-run-task
           :claude-code-adapter
           :http-worker-adapter
           :*claude-code-binary*
           :*claude-code-max-turns*
           :*claude-code-extra-args*
           :*hermes-url*
           :*task-timeout*
           :*adapters*
           :le-worker-claude-code
           :le-worker-hermes))
(in-package :yeditor-enterprise/workers)

;;;; Worker adapters (plan T17, redefined by amendment 4): not chat panels but the
;;;; plugs through which external agents serve the agent host. Each adapter is a
;;;; component: installing it registers a worker on the le-mcp bus (T19) and starts a
;;;; dispatcher that takes the worker's tasks, runs them in the external agent and
;;;; reports the outcome as an observation. Uninstalling stops it and unregisters.
;;;; Credentials stay with the external agent (the Claude Code CLI keeps its own
;;;; login); the editor never handles them.

(defparameter *task-timeout* 600 "Seconds an external agent may spend on one task.")

(defclass worker-adapter ()
  ((name :initarg :name :reader adapter-name)
   (capabilities :initarg :capabilities :initform (list "tasks") :reader adapter-capabilities)
   (worker-id :initform nil :accessor adapter-worker-id)
   (thread :initform nil :accessor adapter-thread)
   (stop :initform nil :accessor adapter-stop-p)))

(defgeneric adapter-ping (adapter)
  (:documentation "Discovery: a short description (version) if the agent is reachable; else nil."))
(defgeneric adapter-run-task (adapter kind payload)
  (:documentation "Run one task in the external agent; returns (values status observation)."))

(defvar *adapters* nil "Adapters currently serving the bus.")

;;; Dispatcher

(defun serve (adapter)
  (loop :until (adapter-stop-p adapter)
        :do (let ((task (ignore-errors (next-task (adapter-worker-id adapter)))))
              (if task
                  (multiple-value-bind (status observation)
                      (handler-case (adapter-run-task adapter (getf task :kind) (getf task :payload))
                        (error (e) (values "failed" (princ-to-string e))))
                    (ignore-errors (report-task (adapter-worker-id adapter) (getf task :id) status observation)))
                  (sleep 0.05)))))

(defun start-adapter (adapter)
  (setf (adapter-worker-id adapter)
        (register-worker (adapter-name adapter) *worker-protocol* (adapter-capabilities adapter))
        (adapter-stop-p adapter) nil
        (adapter-thread adapter)
        (bt2:make-thread (lambda () (serve adapter))
                         :name (format nil "yeditor-worker ~A" (adapter-name adapter))))
  (push adapter *adapters*))

(defun stop-adapter (adapter)
  (setf (adapter-stop-p adapter) t)
  (when (adapter-thread adapter)
    (ignore-errors (bt2:join-thread (adapter-thread adapter))))
  (when (adapter-worker-id adapter)
    (unregister-worker (adapter-worker-id adapter)))
  (setf (adapter-worker-id adapter) nil
        (adapter-thread adapter) nil
        *adapters* (remove adapter *adapters*)))

(defun install-adapter (comp make-adapter)
  "Serve the bus with the adapter built by MAKE-ADAPTER as a transaction effect."
  (let ((adapter nil))
    (yeditor-tx:install-effect comp :worker
                               (lambda () (setf adapter (funcall make-adapter)) (start-adapter adapter))
                               :undo-thunk (lambda () (when adapter (stop-adapter adapter) (setf adapter nil))))))

;;; Running a program with a timeout

(defun run-program-with-timeout (command &key directory (timeout *task-timeout*)
                                             (cancelled-p (constantly nil)))
  "Run COMMAND off the UI thread; reap it on timeout, cancellation or unwinding."
  (let ((process nil) (reader nil) (err-reader nil)
        (out nil) (err nil) (code nil)
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (unwind-protect
         (progn
           (setf process (uiop:launch-program command :directory directory
                                                       :output :stream :error-output :stream))
           (setf reader
                 (bt2:make-thread
                  (lambda ()
                    (with-open-stream (stream (uiop:process-info-output process))
                      (setf out (uiop:slurp-stream-string stream))))))
           (setf err-reader
                 (bt2:make-thread
                  (lambda ()
                    (with-open-stream (stream (uiop:process-info-error-output process))
                      (setf err (uiop:slurp-stream-string stream))))))
           (loop :while (and (uiop:process-alive-p process)
                             (< (get-internal-real-time) deadline)
                             (not (funcall cancelled-p)))
                 :do (sleep 0.02)))
      (when process
        (when (uiop:process-alive-p process) (uiop:terminate-process process :urgent t))
        (setf code (uiop:wait-process process)))
      (when reader (bt2:join-thread reader))
      (when err-reader (bt2:join-thread err-reader)))
    (values out err code)))

;;; Claude Code

(defparameter *claude-code-binary* "claude" "The Claude Code CLI.")
(defparameter *claude-code-max-turns* 20 "Step budget per task (--max-turns).")
(defparameter *claude-code-extra-args* nil "Extra CLI arguments (e.g. --allowedTools ...).")

(defclass claude-code-adapter (worker-adapter)
  ((directory :initarg :directory :initform nil :reader adapter-directory))
  (:default-initargs :name "claude-code" :capabilities (list "tasks" "events")))

(defun mcp-config-file ()
  "A Claude Code MCP config pointing at the editor's own MCP server, so the agent edits
   through editor buffers instead of writing files behind the editor's back."
  (let ((path (uiop:tmpize-pathname (merge-pathnames "yeditor-mcp.json" (uiop:default-temporary-directory)))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "{\"mcpServers\":{\"yeditor\":{\"type\":\"http\",\"url\":\"http://~A:~D/mcp\"}}}"
              *mcp-hostname* *mcp-port*))
    path))

(defmethod adapter-ping ((a claude-code-adapter))
  (multiple-value-bind (out err code)
      (ignore-errors (run-program-with-timeout (list *claude-code-binary* "--version") :timeout 15))
    (declare (ignore err))
    (and code (zerop code) (string-trim '(#\Newline #\Space) out))))

(defmethod adapter-run-task ((a claude-code-adapter) kind payload)
  (let ((config (mcp-config-file)))
    (unwind-protect
         (multiple-value-bind (out err code)
             (run-program-with-timeout
              (append (list *claude-code-binary* "-p" (format nil "[~A] ~A" kind payload)
                            "--output-format" "json"
                            "--max-turns" (princ-to-string *claude-code-max-turns*)
                            "--mcp-config" (namestring config))
                      *claude-code-extra-args*)
              :directory (adapter-directory a)
              :cancelled-p (lambda () (adapter-stop-p a)))
           (let ((result (ignore-errors (yason:parse out))))
             (cond ((and (hash-table-p result) (not (eq (gethash "is_error" result) t)) (zerop code))
                    (values "done" (gethash "result" result)))
                   (t (values "failed" (string-trim '(#\Newline) (or (and (hash-table-p result) (gethash "result" result))
                                                                     (and (plusp (length err)) err)
                                                                     out)))))))
      (ignore-errors (delete-file config)))))

;;; HTTP worker (Hermes Agent and other agents exposing an HTTP API)
;;; Contract (provisional, plan T17 leaves the Hermes API unspecified):
;;;   GET  <url>/health          -> 200 with a short description
;;;   POST <url>/tasks           {"kind","payload"} -> {"status","observation"}

(defparameter *hermes-url* "http://127.0.0.1:8765")

(defclass http-worker-adapter (worker-adapter)
  ((url :initarg :url :reader adapter-url))
  (:default-initargs :name "hermes" :capabilities (list "tasks")))

(defmethod adapter-ping ((a http-worker-adapter))
  (ignore-errors (dex:get (format nil "~A/health" (adapter-url a)) :connect-timeout 2 :read-timeout 2
                                                                   :keep-alive nil)))

(defmethod adapter-run-task ((a http-worker-adapter) kind payload)
  (let* ((body (with-output-to-string (s)
                 (yason:encode (let ((h (make-hash-table :test (function equal))))
                                 (setf (gethash "kind" h) kind (gethash "payload" h) payload) h)
                               s)))
         (response (yason:parse (dex:post (format nil "~A/tasks" (adapter-url a))
                                          :content body :headers (list (cons "content-type" "application/json"))
                                          :read-timeout *task-timeout* :keep-alive nil))))
    (values (or (gethash "status" response) "done") (gethash "observation" response))))

;;; Components

(define-component le-worker-claude-code
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-adapter comp (lambda () (make-instance (quote claude-code-adapter)))))))

(define-component le-worker-hermes
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-adapter comp (lambda () (make-instance (quote http-worker-adapter) :url *hermes-url*))))))
