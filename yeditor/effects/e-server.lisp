;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-server
  (:use :cl :yeditor-tx)
  (:export :install-tcp-server))
(in-package :yeditor-effects/e-server)

(defstruct server-state
  socket
  (active t)
  (lock (bt2:make-lock :name "yeditor TCP server"))
  connections)

(defun close-connection (connection)
  ;; Shutdown wakes readers before closing their streams.
  (ignore-errors (usocket:socket-shutdown connection :io))
  (ignore-errors (usocket:socket-close connection)))

(defun stop-server (state)
  (let ((connections nil))
    (bt2:with-lock-held ((server-state-lock state))
      (setf (server-state-active state) nil
            connections (server-state-connections state)
            (server-state-connections state) nil))
    (when (server-state-socket state)
      (ignore-errors (usocket:socket-close (server-state-socket state))))
    ;; Each worker alone closes its stream. Concurrent CLOSE from undo and the
    ;; worker can deadlock on stream locks; shutdown is enough to wake its read.
    (dolist (connection connections)
      (ignore-errors (usocket:socket-shutdown connection :io)))))

(defun accept-connections (state handler name)
  (handler-case
      (loop :while (server-state-active state)
            :when (usocket:wait-for-input (server-state-socket state) :timeout 0.05 :ready-only t)
              :do (let ((connection (usocket:socket-accept (server-state-socket state))))
                    (bt2:with-lock-held ((server-state-lock state))
                      (if (server-state-active state)
                          (progn
                            (push connection (server-state-connections state))
                            (bt2:make-thread
                             (lambda ()
                               (unwind-protect
                                    (handler-case (funcall handler (usocket:socket-stream connection))
                                      (error (condition)
                                        (when (server-state-active state)
                                          (warn "TCP handler failed: ~A" condition))))
                                 (bt2:with-lock-held ((server-state-lock state))
                                   (setf (server-state-connections state)
                                         (delete connection (server-state-connections state))))
                                 (close-connection connection)))
                             :name (format nil "~A/connection" name)))
                          (close-connection connection)))))
    (error (condition)
      (when (server-state-active state)
        (stop-server state)
        (warn "TCP acceptor failed: ~A" condition)))))

(defun install-tcp-server (comp port handler &key (interface "127.0.0.1"))
  "Listen on PORT and dispatch HANDLER on worker threads.
   Undo closes the listener and accepted connections, unblocking socket readers.
   Each activation owns a separate state. :SOCKET-OF returns the current listener,
   allowing the caller to discover the assigned port when PORT is zero."
  (let ((state nil))
    (labels ((undo ()
               (when state (stop-server state) (setf state nil)))
             (activate ()
               (handler-case
                   (let* ((listener (usocket:socket-listen interface port :reuse-address t
                                                          :element-type '(unsigned-byte 8)))
                          (activation (make-server-state :socket listener))
                          (name (format nil "yeditor-tcp-server:~A" (usocket:get-local-port listener))))
                     (setf state activation)
                     (bt2:make-thread (lambda () (accept-connections activation handler name))
                                      :name name))
                 (error (condition) (undo) (error condition)))))
      (let ((handle (install-effect comp :server #'activate :undo-thunk #'undo)))
        (push (cons :port port) handle)
        (push (cons :socket-of (lambda () (when state (server-state-socket state)))) handle)
        handle))))
