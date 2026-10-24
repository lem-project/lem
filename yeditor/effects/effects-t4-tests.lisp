;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects-t4-tests
  (:use :cl :rove))
(in-package :yeditor-effects-t4-tests)

(defvar *hook* nil)

(lem:define-major-mode t4-mode nil (:name "T4"))
(lem:define-major-mode external-mode nil (:name "External"))
(lem:define-minor-mode t4-minor-mode (:name "T4 minor"))

(defmacro with-tx (&body body)
  `(let ((yeditor-tx:*tx-pending* nil) (yeditor-tx:*tx-committed* nil)) ,@body))

(deftest existing-buffer-is-not-owned
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (lem:make-buffer "*t4-existing*")))
      (unwind-protect
           (with-tx
             (yeditor-effects:install-buffer :test "*t4-existing*")
             (yeditor-tx:tx-commit)
             (yeditor-tx:tx-rollback)
             (ok (eq buffer (lem:get-buffer "*t4-existing*"))
                 "rollback keeps a pre-existing buffer")
             (ok (not (lem:deleted-buffer-p buffer))))
        (unless (lem:deleted-buffer-p buffer) (lem:delete-buffer buffer))))))

(deftest sole-owned-buffer-is-removed
  (lem:with-current-buffers ()
    (with-tx
      (yeditor-effects:install-buffer :test "*t4-sole*")
      (yeditor-tx:tx-commit)
      (ok (= 1 (length (lem:buffer-list))))
      (yeditor-tx:tx-rollback)
      (ok (null (lem:buffer-list)) "rollback removes even the sole owned buffer"))))

(deftest existing-hook-is-not-owned
  (let* ((callback (lambda (&rest args) (declare (ignore args))))
         (*hook* (list (cons callback 17)))
         (before (copy-tree *hook*)))
    (with-tx
      (yeditor-effects:install-hook :test '*hook* callback)
      (yeditor-tx:tx-commit)
      (yeditor-tx:tx-rollback)
      (ok (equal before *hook*) "pre-existing callback and weight survive rollback"))))

(deftest mode-baseline-is-captured-at-commit
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (lem:make-buffer "*t4-mode-baseline*")))
      (unwind-protect
           (with-tx
             (yeditor-effects:install-mode :test buffer 't4-mode)
             (lem:change-buffer-mode buffer 'external-mode)
             (yeditor-tx:tx-commit)
             (ok (eq 't4-mode (lem:buffer-major-mode buffer)))
             (yeditor-tx:tx-rollback)
             (ok (eq 'external-mode (lem:buffer-major-mode buffer))
                 "undo restores the mode present immediately before commit"))
        (lem:delete-buffer buffer)))))

(deftest minor-mode-activation-is-reversible
  (lem-fake-interface:with-fake-interface ()
    (dolist (already-active-p '(nil t))
      (let ((buffer (lem:make-buffer "*t4-minor*")))
        (unwind-protect
             (with-tx
               (when already-active-p
                 (lem:change-buffer-mode buffer 't4-minor-mode t))
               (yeditor-effects:install-mode :test buffer 't4-minor-mode)
               (yeditor-tx:tx-commit)
               (ok (lem:mode-active-p buffer 't4-minor-mode))
               (yeditor-tx:tx-rollback)
               (ok (eq already-active-p (lem:mode-active-p buffer 't4-minor-mode))
                   "restore the previous minor-mode activation"))
          (lem:delete-buffer buffer))))))

(deftest three-effects-300-toggle-cycles
  (lem-fake-interface:with-fake-interface ()
    (let* ((yeditor-context:*component-registry* (make-hash-table :test 'eq))
           (*hook* nil)
           (before-buffers (copy-list (lem:buffer-list)))
           (before-current (lem:current-buffer))
           (callback (lambda (&rest args) (declare (ignore args))))
           (name "*t4-component*"))
      (yeditor-context:register-component
       't4-component
       :install (lambda (component context)
                  (declare (ignore context))
                  (list (yeditor-effects:install-buffer component name)
                        ;; Resolve the freshly created buffer when the effect runs.
                        (yeditor-effects:install-mode component name 't4-mode)
                        (yeditor-effects:install-hook component '*hook* callback))))
      (unwind-protect
           (let* ((entry (yeditor-context:install-component 't4-component))
                  (handles (getf entry :state)))
             (dotimes (cycle 300)
               (ok (= 3 (length (getf entry :state))))
               (ok (eq 't4-mode (lem:buffer-major-mode (lem:get-buffer name))))
               (ok (= 1 (length *hook*)))
               (yeditor-context:toggle-component 't4-component)
               (ok (null (getf entry :state)))
               (ok (= 3 (length (getf entry :suspended))))
               (ok (every #'eq handles (getf entry :suspended)))
               (ok (null (lem:get-buffer name)))
               (ok (equal before-buffers (lem:buffer-list)))
               (ok (null *hook*))
               (ok (eq before-current (lem:current-buffer)))
               (yeditor-context:toggle-component 't4-component)
               (ok (every #'eq handles (getf entry :state))))
             (yeditor-context:uninstall-component 't4-component)
             (ok (null (getf entry :state)))
             (ok (null (getf entry :suspended)))
             (ok (equal before-buffers (lem:buffer-list)))
             (ok (null *hook*)))
        (ignore-errors (yeditor-context:uninstall-component 't4-component))
        (let ((buffer (lem:get-buffer name)))
          (when buffer (lem:delete-buffer buffer)))))))
