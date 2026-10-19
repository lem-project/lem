;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects-t6-lifecycle-tests
  (:use :cl :rove :yeditor-tx :yeditor-effects))
(in-package :yeditor-effects-t6-lifecycle-tests)

(defclass event-timer-manager (lem/common/timer:timer-manager) ())
(defmethod lem/common/timer:send-timer-notification ((manager event-timer-manager) callback)
  (lem:send-event callback))

(defun wait-until (predicate &key (events nil) (timeout 3))
  (loop :with deadline := (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        :thereis (funcall predicate)
        :while (< (get-internal-real-time) deadline)
        :do (if events (lem:receive-event 0.01) (sleep 0.01))))

(defun inverse (handle) (funcall (cdr (assoc :inverse handle))))
(defun resource (handle key) (funcall (cdr (assoc key handle))))

(deftest function-baselines-and-nested-overrides
  (let ((*tx-pending* nil) (*tx-committed* nil) (symbol (gensym "T6-FUNCTION")))
    (unwind-protect
         (progn
           (install-fdefinition 'queued symbol (lambda () :active))
           (setf (fdefinition symbol) (lambda () :baseline))
           (handler-bind ((warning #'muffle-warning)) (tx-commit))
           (tx-rollback)
           (ok (and (fboundp symbol) (eq (funcall symbol) :baseline))
               "snapshot is captured at activation")
           (setf (fdefinition symbol) (lambda () :baseline))
           (let ((first (install-fdefinition 'first symbol (lambda () :first)))
                 (second (install-fdefinition 'second symbol (lambda () :second))))
             (handler-bind ((warning #'muffle-warning)) (tx-commit))
             (inverse second)
             (ok (eq (funcall symbol) :first) "inner inverse restores outer override")
             (inverse first)
             (ok (eq (funcall symbol) :baseline))))
      (fmakunbound symbol))))

(deftest process-output-and-buffer-ownership
  (lem-fake-interface:with-fake-interface ()
    (let* ((*tx-pending* nil) (*tx-committed* nil)
           (buffer (lem:make-buffer "*t6-borrowed-output*"))
           (handle (install-process 'output
                                   '("python3" "-u" "-c" "import time; print('agent-ready'); time.sleep(60)")
                                   :output-buffer buffer)))
      (unwind-protect
           (progn
             (tx-commit)
             (ok (wait-until (lambda () (search "agent-ready" (lem:buffer-text buffer)))
                             :events t :timeout 1)
                 "stdout reaches the editor through its event queue")
             (let ((process (resource handle :process-of)))
               (tx-rollback)
               (ok (wait-until (lambda () (not (uiop:process-alive-p process)))))
             (inverse handle)
             (ok (eq buffer (lem:get-buffer "*t6-borrowed-output*"))
                 "a preexisting output buffer survives rollback")
             (ok (null (resource handle :process-of)) "handle releases process ownership")))
        (inverse handle)
        (when (lem:get-buffer "*t6-borrowed-output*") (lem:delete-buffer buffer))))))

(deftest dummy-process-component-100-cycles
  (lem-fake-interface:with-fake-interface ()
    (let ((yeditor-context::*component-registry* (make-hash-table))
          (handle nil))
      (yeditor-context:register-component
       'dummy-process :install
       (lambda (comp context)
         (declare (ignore context))
         (list (setf handle (install-process comp
                                            '("python3" "-c" "import time; time.sleep(1)")
                                            :output-buffer "*t6-owned-output*")))))
      (unwind-protect
           (dotimes (i 100)
             (yeditor-context:install-component 'dummy-process)
             (let* ((process (resource handle :process-of))
                    (pid (uiop:process-info-pid process)))
               (ok (uiop:process-alive-p process))
               (ok (lem:get-buffer "*t6-owned-output*"))
               (yeditor-context:uninstall-component 'dummy-process)
               (ok (wait-until (lambda () (not (uiop:process-alive-p process)))))
               (ok (not (zerop (nth-value 2 (uiop:run-program (list "ps" "-p" (write-to-string pid))
                                                :output nil :error-output nil
                                                :ignore-error-status t))))
                   "ps confirms the child was reaped")
               (ok (null (lem:get-buffer "*t6-owned-output*"))))
             (yeditor-context:uninstall-component 'dummy-process))
        (yeditor-context:uninstall-component 'dummy-process)))))

(defun free-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port socket) (usocket:socket-close socket))))

(deftest server-closes-connections-and-workers
  (let* ((*tx-pending* nil) (*tx-committed* nil)
         (port (free-port))
         (handle (install-tcp-server 'server port
                    (lambda (stream)
                      (write-byte (read-byte stream) stream)
                      (finish-output stream)
                      (read-byte stream nil nil))))
         (connection nil))
    (unwind-protect
         (progn
           (tx-commit)
           (setf connection (usocket:socket-connect "127.0.0.1" port
                                                   :element-type '(unsigned-byte 8)))
           (let ((stream (usocket:socket-stream connection)))
             (write-byte 42 stream) (finish-output stream)
             (ok (usocket:wait-for-input connection :timeout 1 :ready-only t))
             (ok (= 42 (read-byte stream)))
             (tx-rollback)
             (ok (usocket:wait-for-input connection :timeout 1 :ready-only t)
                 "rollback closes an accepted connection blocked in the handler")
             (when (listen stream) (read-byte stream nil nil)))
           (ok (wait-until
                (lambda ()
                  (notany (lambda (thread)
                            (search (format nil "yeditor-tcp-server:~A" port)
                                    (bt2:thread-name thread)))
                          (bt2:all-threads))) :timeout 1)
               "acceptor and handler threads exit")
           (inverse handle))
      (when connection (ignore-errors (usocket:socket-close connection)))
      (inverse handle))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (lem:define-major-mode t6-language-mode lem/language-mode:language-mode (:name "T6")))

(lem-lsp-mode:define-language-spec (t6-language-spec t6-language-mode)
  :language-id "t6-lifecycle"
  :command (list "python3" (namestring (asdf:system-relative-pathname
                                      "yeditor-effects" "../components/test-data/fake-lsp-server.py")))
  :connection-mode :stdio)

(deftest lsp-preserves-borrowed-session-and-cleans-owned-session
  (lem/common/timer:with-timer-manager (make-instance 'event-timer-manager)
   (lem-fake-interface:with-fake-interface ()
    (let* ((*tx-pending* nil) (*tx-committed* nil)
           (buffer (lem:make-buffer "*t6-language*"))
           (lem-lsp-mode:*disable* t)
           (workspace nil))
      (lem:change-buffer-mode buffer 't6-language-mode)
      (unwind-protect
           (dotimes (i 100)
             (let ((owner (install-lsp-session 'owner buffer)))
               (tx-commit)
               (setf workspace (resource owner :workspace-of))
               (ok (lem-lsp-mode:lsp-session-alive-p workspace))
               (let ((borrower (install-lsp-session 'borrower buffer)))
                 (tx-commit)
                 (ok (eq workspace (resource borrower :workspace-of))
                     "a pending or initialized workspace is reused")
                 (inverse borrower)
                 (ok (lem-lsp-mode:lsp-session-alive-p workspace)
                     "borrower's inverse preserves the owner's session"))
               (inverse owner)
               (inverse owner)
               (ok (not (lem-lsp-mode:lsp-session-alive-p workspace)))
               (ok (null (resource owner :workspace-of)))
               ;; Execute queued connection callbacks after stop: no resurrection.
               (lem:receive-event 0)))
        (when workspace (lem-lsp-mode:stop-lsp-session workspace))
        (lem:delete-buffer buffer))))))

(deftest server-100-reactivations
  (let* ((*tx-pending* nil) (*tx-committed* nil)
         (handle (install-tcp-server 'echo 0
                   (lambda (stream)
                     (write-byte (read-byte stream) stream)
                     (finish-output stream)))))
    (unwind-protect
         (dotimes (i 100)
           (if (zerop i) (tx-commit) (funcall (cdr (assoc :fn handle))))
           (let* ((listener (resource handle :socket-of))
                  (port (usocket:get-local-port listener))
                  (connection (usocket:socket-connect "127.0.0.1" port
                                                      :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (let ((stream (usocket:socket-stream connection)))
                    (write-byte 17 stream) (finish-output stream)
                    (ok (usocket:wait-for-input connection :timeout 1 :ready-only t))
                    (ok (= 17 (read-byte stream))))
               (usocket:socket-close connection))
             (inverse handle)
             (inverse handle)
             (ok (null (resource handle :socket-of)))
             (ok (wait-until
                  (lambda ()
                    (notany (lambda (thread)
                              (search (format nil "yeditor-tcp-server:~A" port)
                                      (bt2:thread-name thread)))
                            (bt2:all-threads))) :timeout 2)
                 "each activation releases its acceptor and workers")))
      (inverse handle))))

(deftest lsp-initialization-and-pending-callback-cancellation
  (lem/common/timer:with-timer-manager (make-instance 'event-timer-manager)
   (lem-fake-interface:with-fake-interface ()
    (let* ((*tx-pending* nil) (*tx-committed* nil)
           (buffer (lem:make-buffer "*t6-initialize*"))
           (lem-lsp-mode:*disable* t)
           (ready nil) (handle nil))
      (lem:change-buffer-mode buffer 't6-language-mode)
      (unwind-protect
           (progn
             (setf handle (install-lsp-session 'initialized buffer))
             (tx-commit)
             (let ((workspace (resource handle :workspace-of)))
               (multiple-value-bind (reused created)
                   (lem-lsp-mode:start-lsp-session buffer :then (lambda () (setf ready t)))
                 (ok (eq reused workspace))
                 (ok (null created))
                 (ok (null ready) "reused pending session delays THEN until initialize"))
               (ok (wait-until (lambda () ready) :events t)
                   "the real stdio fixture completes LSP initialize")
               (inverse handle)
               (ok (not (lem-lsp-mode:lsp-session-alive-p workspace))))
             ;; A pending continuation is cancelled if stop wins the race.
             (funcall (cdr (assoc :fn handle)))
             (setf ready nil)
             (lem-lsp-mode:start-lsp-session buffer :then (lambda () (setf ready t)))
             (inverse handle)
             (lem:receive-event 0.1)
             (ok (null ready) "stopped initialization cannot call THEN"))
        (when handle (inverse handle))
        (lem:delete-buffer buffer))))))
