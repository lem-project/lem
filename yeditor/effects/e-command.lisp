;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-command
  (:use :cl :lem :yeditor-tx)
  (:export :install-command))
(in-package :yeditor-effects/e-command)

(defclass yeditor-dynamic-command (primary-command)
  ((fn :initarg :fn :reader dynamic-command-fn)))

(defmethod execute (mode (command yeditor-dynamic-command) argument)
  (declare (ignore mode argument))
  (funcall (dynamic-command-fn command)))

(defun install-command (comp name fn)
  "Register a dynamic command NAME (string) whose body is FN, as a transaction
   effect (kind 7, Cap. 5.3). Undo removes it or restores the previous definition."
  (let* ((prev (find-command name))
         (instance (make-instance 'yeditor-dynamic-command :name name :fn fn))
         (handle (install-effect comp :command
                                 (lambda () (add-command name instance))
                                 :undo-thunk
                                 (lambda ()
                                   (if prev
                                       (add-command name prev)
                                       (remove-command name))))))
    (push (cons :command-name name) handle)
    (push (cons :prev prev) handle)
    handle))
