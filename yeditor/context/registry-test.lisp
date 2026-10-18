;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(in-package :cl-user)

;; NOTE (controller): no runtime (require ...) — the test system chain (registry-tests
;; -> yeditor-context -> yeditor-tx -> rove) provides all packages; entry point is
;; scripts/run-yeditor-registry-tests.lisp.

;; Dummy component definition
(yeditor-context:define-component dummy-component
  :version "1.0"
  :author "test"
  :depends-on nil
  :install (lambda (comp ctx)
             ;; create 10 effects, each installing a dummy var and returning handle
             (let ((handles nil))
               (dotimes (i 10)
                 ;; capture the CURRENT value of i (SBCL dotimes shares the binding)
                 (let ((dummy-sym (intern (format nil "*DUMMY~A*" i))))
                   (let ((h (yeditor-tx:install-effect comp :dummy
                            (lambda () (setf (symbol-value dummy-sym) t))
                            :undo-thunk (lambda () (setf (symbol-value dummy-sym) nil)))))
                     (push h handles))))
               (reverse handles)))
  :activate nil
  :deactivate nil
  :config-spec nil)

(defun clear-dummy-vars ()
  (dotimes (i 10)
    (setf (symbol-value (intern (format nil "*DUMMY~A*" i))) nil)))

(rove:deftest test-install-and-toggle
  (clear-dummy-vars)
  ;; install
  (let ((comp-entry (yeditor-context:install-component 'dummy-component)))
    (rove:ok (equal (length (getf comp-entry :state)) 10) "install yields 10 handles")
    ;; all dummy vars true
    (dotimes (i 10)
      (rove:ok (symbol-value (intern (format nil "*DUMMY~A*" i)))
               (format nil "dummy var *DUMMY~A* set" i)))
    ;; suspend
    (yeditor-context:toggle-component 'dummy-component)
    ;; after suspend, all dummy vars nil
    (dotimes (i 10)
      (rove:ok (not (symbol-value (intern (format nil "*DUMMY~A*" i))))
               (format nil "dummy var *DUMMY~A* cleared after suspend" i)))
    ;; resume
    (yeditor-context:toggle-component 'dummy-component)
    (dotimes (i 10)
      (rove:ok (symbol-value (intern (format nil "*DUMMY~A*" i)))
               (format nil "dummy var *DUMMY~A* restored after resume" i))))
  ;; 1000 toggle loop
  (dotimes (n 1000)
    (yeditor-context:toggle-component 'dummy-component)
    (dotimes (i 10)
      ;; previous round ends RESUMED; toggle #0 suspends (nil), toggle #1 resumes (t), ...
      (if (evenp n)
          (rove:ok (not (symbol-value (intern (format nil "*DUMMY~A*" i)))))
          (rove:ok (symbol-value (intern (format nil "*DUMMY~A*" i)))))))
  (clear-dummy-vars))

;; Test install error rollback
(yeditor-context:define-component faulty-component
  :version "1.0"
  :author "test"
  :depends-on nil
  :install (lambda (comp ctx)
             (yeditor-tx:install-effect comp :bad (lambda () (error "boom")) :undo-thunk (lambda ()))
             nil)
  :activate nil
  :deactivate nil
  :config-spec nil)

(rove:deftest test-install-error
  (handler-case
      (yeditor-context:install-component 'faulty-component)
    (error (e)
      (rove:ok (string= (string (type-of e)) "SIMPLE-ERROR") "error signaled"))
    (:no-error ()
      (rove:fail "install should have signaled error"))))

;; Test dependency missing
(yeditor-context:define-component dependent-component
  :version "1.0"
  :author "test"
  :depends-on '(nonexistent)
  :install (lambda (c ctx) nil)
  :activate nil
  :deactivate nil
  :config-spec nil)

(rove:deftest test-missing-dep
  (handler-case
      (yeditor-context:install-component 'dependent-component)
    (error (e)
      (rove:ok (search "nonexistent" (princ-to-string e) :test #'string-equal) "missing dependency reported"))
    (:no-error ()
      (rove:fail "install should have failed due to missing dep"))))

;; Test idempotent uninstall
(rove:deftest test-uninstall-idempotent
  (yeditor-context:install-component 'dummy-component)
  (yeditor-context:uninstall-component 'dummy-component)
  (rove:ok (null (getf (gethash 'dummy-component yeditor-context::*component-registry*) :state)) "state nil after uninstall")
  (yeditor-context:uninstall-component 'dummy-component) ; second call should be safe
  (rove:ok t "second uninstall no error"))

(rove:deftest test-toggle-preserves-handle-identity
  (clear-dummy-vars)
  (yeditor-context:uninstall-component 'dummy-component)
  (let* ((entry (yeditor-context:install-component 'dummy-component))
         (before (copy-list (getf (gethash 'dummy-component yeditor-context:*component-registry*) :state))))
    (declare (ignore entry))
    (dotimes (n 100)
      (yeditor-context:toggle-component 'dummy-component)
      (yeditor-context:toggle-component 'dummy-component))
    (let ((after (getf (gethash 'dummy-component yeditor-context:*component-registry*) :state)))
      (rove:ok (= 10 (length after)) "10 handles after 100 suspend/resume cycles")
      (rove:ok (every #'eq before after) "same handle objects (identity preserved)")))
  (yeditor-context:uninstall-component 'dummy-component)
  (clear-dummy-vars))

(defvar *outer-ran* nil)

(rove:deftest test-nested-transaction-isolated
  ;; an inner install-component must not commit effects pending in an outer tx
  (setf *outer-ran* nil)
  (yeditor-context:uninstall-component 'dummy-component)
  (yeditor-tx:with-transaction
    (yeditor-tx:install-effect 'outer :noop (lambda () (setf *outer-ran* t))
                               :undo-thunk (lambda () (setf *outer-ran* nil)))
    (yeditor-context:install-component 'dummy-component)
    (rove:ok (not *outer-ran*) "outer pending effect not run by inner commit")
    (yeditor-tx:tx-commit)
    (rove:ok *outer-ran* "outer effect runs on its own commit"))
  (yeditor-context:uninstall-component 'dummy-component)
  (clear-dummy-vars))
