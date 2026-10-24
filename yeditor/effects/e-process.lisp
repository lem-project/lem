;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-process
  (:use :cl :yeditor-tx)
  (:export :install-process))
(in-package :yeditor-effects/e-process)

(defun collect-output (process buffer active)
  "Drain stdout off the UI thread, dispatch editor writes, and reap the child."
  (let ((stream (uiop:process-info-output process)))
    (unwind-protect
         (when stream
           (loop :for first := (read-char stream nil)
                 :while first
                 :for text := (with-output-to-string (out)
                                (write-char first out)
                                (loop :while (listen stream)
                                      :for char := (read-char stream nil)
                                      :while char :do (write-char char out)))
                 :do (let ((chunk text))
                       (lem:send-event
                        (lambda ()
                          (when (and (car active)
                                     (eq buffer (lem:get-buffer (lem:buffer-name buffer))))
                            (lem:insert-string (lem:buffer-end-point buffer) chunk)))))))
      (when stream (close stream))
      (uiop:wait-process process))))

(defun install-process (comp command &key output-buffer)
  "Launch COMMAND asynchronously; rollback terminates the child and owned buffer.
   OUTPUT-BUFFER may be a buffer or name. Existing buffers are borrowed; a named
   buffer created by this activation is deleted on undo. Stdout is delivered via
   the editor event queue. :PROCESS-OF returns the current UIOP process or NIL."
  (let ((process nil) (buffer nil) (owned nil) (active nil))
    (labels ((undo ()
               (when active (setf (car active) nil active nil))
               (when process
                 (uiop:terminate-process process :urgent t)
                 (setf process nil))
               (when owned
                 (lem:delete-buffer buffer)
                 (setf owned nil))
               (setf buffer nil))
             (activate ()
               (handler-case
                   (progn
                     (setf active (list t))
                     (when output-buffer
                       (setf buffer
                             (etypecase output-buffer
                               (lem:buffer output-buffer)
                               (string
                                (or (lem:get-buffer output-buffer)
                                    (progn
                                      (setf owned t)
                                      (lem:make-buffer output-buffer :enable-undo-p nil)))))))
                     (setf process (uiop:launch-program command
                                                       :output (when buffer :stream)
                                                       :error-output nil))
                     ;; Captured values belong to this activation, never a resumed one.
                     (let ((child process) (target buffer) (token active))
                       (bt2:make-thread (lambda () (collect-output child target token))
                                        :name "yeditor-process-output")))
                 (error (condition) (undo) (error condition)))))
      (let ((handle (install-effect comp :process #'activate :undo-thunk #'undo)))
        (push (cons :command command) handle)
        (push (cons :process-of (lambda () process)) handle)
        handle))))
