;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects-tests
  (:use :cl :yeditor-context :yeditor-effects :yeditor-tx :rove))
(in-package :yeditor-effects-tests)

(defun dummy-hook-fn (&rest _args) (declare (ignore _args)))

(defun hook-contains-p (hook-var fn)
  (find fn (symbol-value hook-var) :key #'car))

(yeditor-context:define-component test-effects
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-buffer comp "test-buffer")
                   (install-mode comp (lem:current-buffer) (lambda (buf) (declare (ignore buf))))
                   (install-hook comp 'lem:*after-init-hook* #'dummy-hook-fn))))

(deftest install-and-uninstall-buffer
  (let ((comp 'test-effects))
    (yeditor-context:install-component comp)
    (ok (find "test-buffer" (lem:buffer-list) :key #'lem:buffer-name :test #'string=)
        "test buffer created on install")
    (ok (hook-contains-p 'lem:*after-init-hook* #'dummy-hook-fn)
        "hook installed on install")
    (yeditor-context:uninstall-component comp)
    (ok (null (find "test-buffer" (lem:buffer-list) :key #'lem:buffer-name :test #'string=))
        "test buffer removed on uninstall")
    (ok (null (hook-contains-p 'lem:*after-init-hook* #'dummy-hook-fn))
        "hook removed on uninstall")))
