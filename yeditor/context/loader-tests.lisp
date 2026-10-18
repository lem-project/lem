;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-context-loader-tests
  (:use :cl :rove :yeditor-context :yeditor-context/loader))
(in-package :yeditor-context-loader-tests)

(defvar *flag-a* nil)
(defvar *flag-b* nil)
(defvar *flag-c* nil)
(defvar *flag-d* nil)

(defun make-flag-install (var)
  (lambda (comp ctx)
    (declare (ignore ctx))
    (list (yeditor-tx:install-effect comp :noop
                                     (lambda () (setf (symbol-value var) t))
                                     :undo-thunk (lambda () (setf (symbol-value var) nil))))))

(define-component loader-a :version "1" :install (make-flag-install '*flag-a*)
  :config-spec '(:delay))
(define-component loader-b :version "1" :install (make-flag-install '*flag-b*))
(define-component loader-c :version "1" :install (make-flag-install '*flag-c*))
(define-component loader-d :version "1" :install (make-flag-install '*flag-d*))

(defun reset-loader ()
  (dolist (sym (list 'loader-a 'loader-b 'loader-c 'loader-d))
    (ignore-errors (uninstall-component sym)))
  (clrhash *applied-components*)
  (setf *flag-a* nil *flag-b* nil *flag-c* nil *flag-d* nil))

(defun config-path ()
  (merge-pathnames "yeditor-loader-test.lisp" (uiop:default-temporary-directory)))

(defun write-config (components)
  (with-open-file (out (config-path) :direction :output :if-exists :supersede)
    (let ((*package* (find-package :yeditor-context-loader-tests)))
      (dolist (sym components)
        (format out "~S~%" `(use-component ,sym))))))

(deftest test-parse-config-rejects-bad-forms
  (ok (equal (parse-config '((use-component loader-a) (use-component loader-b :delay 3)))
             '((loader-a) (loader-b :delay 3)))
      "well-formed forms parse")
  (ok (handler-case (progn (parse-config '((load "x"))) nil) (error () t))
      "non use-component form rejected")
  (ok (handler-case (progn (parse-config '((use-component loader-a) (use-component loader-a))) nil)
        (error () t))
      "duplicate component rejected"))

(deftest test-config-option-validation
  (reset-loader)
  (let ((errors (apply-config (parse-config '((use-component loader-a :bogus 1))))))
    (ok (= 1 (length errors)) "unknown option reported")
    (ok (not *flag-a*) "component with invalid config not installed")
    (ok (null (nth-value 1 (gethash 'loader-a *applied-components*))) "not recorded as applied"))
  (let ((errors (apply-config (parse-config '((use-component loader-a :delay 5))))))
    (ok (null errors) "declared option accepted")
    (ok *flag-a* "component installed with valid config")))

(deftest test-config-delta-50-mutations
  ;; 50 successive rewrites of the config file; after each apply the installed
  ;; set must equal exactly the set declared in the file (100% of the delta).
  (reset-loader)
  (let ((all '(loader-a loader-b loader-c loader-d))
        (flags (list (cons 'loader-a '*flag-a*) (cons 'loader-b '*flag-b*)
                     (cons 'loader-c '*flag-c*) (cons 'loader-d '*flag-d*))))
    (dotimes (step 50)
      (let* ((mask (mod (* step 7) 16))
             (chosen (loop :for sym :in all
                           :for bit :from 0
                           :when (logbitp bit mask) :collect sym)))
        (write-config chosen)
        (let ((errors (apply-config-file (config-path) :package (find-package :yeditor-context-loader-tests))))
          (ok (null errors) (format nil "step ~A applied without errors" step))
          (dolist (pair flags)
            (ok (eq (and (member (car pair) chosen) t)
                    (and (symbol-value (cdr pair)) t))
                (format nil "step ~A: ~A active iff declared" step (car pair)))))))
    (delete-file (config-path))))

(deftest test-hmr-removed-component-uninstalled
  (reset-loader)
  (write-config '(loader-b loader-c))
  (apply-config-file (config-path) :package (find-package :yeditor-context-loader-tests))
  (ok (and *flag-b* *flag-c*) "both installed from config")
  (write-config '(loader-b))
  (apply-config-file (config-path) :package (find-package :yeditor-context-loader-tests))
  (ok (and *flag-b* (not *flag-c*)) "component removed from file is uninstalled")
  (delete-file (config-path)))
