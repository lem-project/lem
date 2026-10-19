;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-server
  (:use :cl :yeditor-tx)
  (:export :install-tcp-server))
(in-package :yeditor-effects/e-server)

(defun install-tcp-server (comp port handler &key (interface "127.0.0.1"))
  "Open a TCP listener on PORT as a transaction effect (kind 11, Cap. 5.3).
   HANDLER is a function (stream) called for each accepted connection.
   INTERFACE defaults to loopback.  Undo closes the listener socket.
   Uses usocket (already a Quicklisp dependency via lem-socket-utils)."
  (let ((socket nil))
    (let ((handle (install-effect comp :server
                                   (lambda ()
                                     (setf socket
                                           (usocket:socket-listen interface port
                                                                   :reuse-address t
                                                                   :element-type '(unsigned-byte 8)))
                                     ;; spawn acceptor thread
                                     (sb-thread:make-thread
                                      (lambda ()
                                        (handler-case
                                            (loop
                                              (let ((conn (usocket:socket-accept socket)))
                                                (sb-thread:make-thread
                                                 (lambda ()
                                                   (unwind-protect
                                                        (funcall handler (usocket:socket-stream conn))
                                                     (usocket:socket-close conn))))))
                                          (error () nil)))
                                      :name (format nil "yeditor-tcp-server:~A" port)))
                                   :undo-thunk
                                   (lambda ()
                                     (when socket
                                       (handler-case (usocket:socket-close socket)
                                         (error () nil)))))))
      (push (cons :port port) handle)
      handle)))
