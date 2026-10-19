;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/context-bridge-tests
  (:use :cl :rove))
(in-package :yeditor-components/context-bridge-tests)

(deftest real-buffer-lifecycle-100-cycles
  ;; Fake frontends share the global buffer manager; use fresh buffers and align
  ;; the current buffer with the newly created frame before switching/deleting.
  (lem:save-excursion
   (lem/buffer/buffer-list-manager:with-current-buffers ()
    (lem-fake-interface:with-fake-interface ()
     (setf (lem:current-buffer) (lem:window-buffer (lem:current-window)))
    (let* ((yeditor-context:*component-registry* (copy-hash-table yeditor-context:*component-registry*))
           (yeditor-context/reactivity::*coeffect-registry* (make-hash-table :test #'eq))
           (yeditor-context/reactivity::*current-context* (list :window-width 0))
           (lem-lsp-mode:*disable* t)
           (active nil)
           (before-buffers (copy-list (lem:buffer-list)))
           (before-switch (copy-list lem:*switch-to-buffer-hook*))
           (before-file (copy-list lem:*find-file-hook*))
           (before-mode (copy-list lem:*buffer-mode-change-hook*))
           (before-command (copy-list lem:*post-command-hook*))
           (before-kill (copy-tree (lem:variable-value 'lem:kill-buffer-hook :global)))
           (buffer nil))
      (yeditor-context:register-component 'reactive-python
        :install (lambda (comp ctx) (declare (ignore ctx))
                   (list (yeditor-tx:install-effect comp :active (lambda () (setf active t))
                                                    :undo-thunk (lambda () (setf active nil))))))
      (yeditor-context/reactivity:define-coeffects 'reactive-python
                                                 '(:buffer-mode lem-python-mode:python-mode))
      (unwind-protect
           (progn
             (yeditor-context:install-component 'yeditor-components/context-bridge:le-context-bridge)
             (dotimes (i 100)
               (setf buffer (lem:make-buffer "*reactive-python*"))
               (lem:change-buffer-mode buffer 'lem-python-mode:python-mode)
               (lem:switch-to-buffer buffer)
               (ok active "opening/selecting a real Python buffer activates")
               (lem:change-buffer-mode buffer 'lem-java-mode:java-mode)
               (ok (not active) "changing to Java deactivates without a synthetic event")
               (lem:change-buffer-mode buffer 'lem-python-mode:python-mode)
               (ok active "changing back reactivates")
               (lem:delete-buffer buffer)
               (setf buffer nil)
               (ok (not active) "deleting the selected buffer deactivates")))
        (when buffer (lem:delete-buffer buffer))
        (yeditor-context:uninstall-component 'reactive-python)
        (yeditor-context:uninstall-component 'yeditor-components/context-bridge:le-context-bridge))
      (ok (equal before-buffers (lem:buffer-list)) "no buffer residue")
      (ok (equal before-switch lem:*switch-to-buffer-hook*))
      (ok (equal before-file lem:*find-file-hook*))
      (ok (equal before-mode lem:*buffer-mode-change-hook*))
      (ok (equal before-command lem:*post-command-hook*))
      (ok (equal before-kill (lem:variable-value 'lem:kill-buffer-hook :global))
          "bridge uninstall restores borrowed editor hooks"))))))

(defun copy-hash-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value) (setf (gethash key copy) (copy-list value))) table)
    copy))

(deftest empty-component-state-is-visible-to-agents
  (let ((yeditor-context:*component-registry* (make-hash-table :test #'eq)))
    (yeditor-context:register-component 'empty-agent-component
      :install (lambda (comp ctx) (declare (ignore comp ctx)) nil))
    (yeditor-context:install-component 'empty-agent-component)
    (ok (equal (cdr (assoc "empty-agent-component" (yeditor-components/mcp:component-states)
                           :test #'equal)) "active"))
    (yeditor-context:toggle-component 'empty-agent-component)
    (ok (equal (cdr (assoc "empty-agent-component" (yeditor-components/mcp:component-states)
                           :test #'equal)) "suspended"))
    (yeditor-context:uninstall-component 'empty-agent-component)))
