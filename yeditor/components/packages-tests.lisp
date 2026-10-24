;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/packages-tests
  (:use :cl :rove :lem :yeditor-context :yeditor-components/packages))
(in-package :yeditor-components/packages-tests)

(deftest install-is-idempotent-and-preserves-source-registration
  (let* ((root (yeditor-components-tests::make-dummy-project))
         (dir (merge-pathnames "yeditor-dummy-pkg/" root))
         (asdf:*central-registry* (cons dir asdf:*central-registry*))
         (before (copy-list asdf:*central-registry*)))
    (unwind-protect
         (progn
           (install-package "yeditor-dummy-pkg" :roots (list root))
           (let ((state (getf (gethash (package-component-name "yeditor-dummy-pkg") *component-registry*) :state)))
             (install-package "yeditor-dummy-pkg" :roots (list root))
             (ok (eq state (getf (gethash (package-component-name "yeditor-dummy-pkg") *component-registry*) :state))))
           (uninstall-package "yeditor-dummy-pkg")
           (ok (equal before asdf:*central-registry*))
           (ok (null (find-package :yeditor-dummy-pkg))))
      (when (member "yeditor-dummy-pkg" (installed-packages) :test #'equal)
        (uninstall-package "yeditor-dummy-pkg"))
      (uiop:delete-directory-tree root :validate t))))

(deftest failed-load-cleans-new-packages-and-registration
  (let* ((root (yeditor-components-tests::make-dummy-project))
         (file (merge-pathnames "yeditor-dummy-pkg/main.lisp" root))
         (before (copy-list asdf:*central-registry*)))
    (with-open-file (out file :direction :output :if-exists :append)
      (write-line "(error \"fixture load failure\")" out))
    (unwind-protect
         (progn
           (ok (signals (install-package "yeditor-dummy-pkg" :roots (list root))))
           (ok (null (find-package :yeditor-dummy-pkg)))
           (ok (null (gethash (package-component-name "yeditor-dummy-pkg") *component-registry*)))
           (ok (null (asdf:registered-system "yeditor-dummy-pkg")))
           (ok (equal before asdf:*central-registry*)))
      (when (find-package :yeditor-dummy-pkg) (delete-package :yeditor-dummy-pkg))
      (asdf:clear-system "yeditor-dummy-pkg")
      (uiop:delete-directory-tree root :validate t))))

(deftest scan-the-active-quicklisp-local-projects
  (let* ((root (yeditor-components-tests::make-dummy-project))
         (symbol (and (find-package "QL") (find-symbol "*LOCAL-PROJECT-DIRECTORIES*" "QL"))))
    (unwind-protect
         (when symbol
           (progv (list symbol) (list (list root))
             (let ((*package-roots* nil))
               (ok (assoc "yeditor-dummy-pkg" (scan-local-systems) :test #'equal)))))
      (uiop:delete-directory-tree root :validate t))))

(deftest palette-install-remove-100-cycles
  (let ((before yeditor-components/palette:*palette-actions*))
    (dotimes (i 100)
      (install-component 'le-package-onboarding)
      (ok (assoc "Install package..." yeditor-components/palette:*palette-actions* :test #'equal))
      (ok (assoc "Remove package..." yeditor-components/palette:*palette-actions* :test #'equal))
      (uninstall-component 'le-package-onboarding)
      (ok (eq before yeditor-components/palette:*palette-actions*)))))

(deftest package-task-delivers-results-on-the-ui-thread
  (lem-fake-interface:with-fake-interface ()
    (let ((ui-thread (bt2:current-thread)) (delivered nil))
      (let ((worker (yeditor-components/packages::run-package-task
                     (lambda () (not (eq ui-thread (bt2:current-thread))))
                     (lambda (value)
                       (setf delivered (and value (eq ui-thread (bt2:current-thread))))))))
        (bt2:join-thread worker)
        (ok (null delivered) "work completes without running UI callbacks in the worker")
        (ok (signals (yeditor-components/packages::run-package-task (constantly nil) #'identity))
            "a second operation waits for completion")
        (ok (yeditor-components/lsp-tests::wait-for (lambda () delivered)))
        (ok (null yeditor-components/packages::*package-job*))))))

(deftest failed-package-task-releases-the-worker-slot
  (lem-fake-interface:with-fake-interface ()
    (let ((succeeded nil))
      (bt2:join-thread
       (yeditor-components/packages::run-package-task
        (lambda () (error "fixture asynchronous failure"))
        (lambda (result) (declare (ignore result)) (setf succeeded t))))
      (ok (yeditor-components/lsp-tests::wait-for
           (lambda () (null yeditor-components/packages::*package-job*))))
      (ok (null succeeded)))))

(deftest load-and-remove-a-package-through-the-worker
  (lem-fake-interface:with-fake-interface ()
    (let ((root (yeditor-components-tests::make-dummy-project)) (completed nil))
      (unwind-protect
           (progn
             (yeditor-components/packages::run-package-task
              (lambda () (install-package "yeditor-dummy-pkg" :roots (list root)))
              (lambda (component) (setf completed component)))
             (ok (yeditor-components/lsp-tests::wait-for (lambda () completed)))
             (ok (find-package :yeditor-dummy-pkg))
             (setf completed nil)
             (yeditor-components/packages::run-package-task
              (lambda () (uninstall-package "yeditor-dummy-pkg"))
              (lambda (result) (setf completed result)))
             (ok (yeditor-components/lsp-tests::wait-for (lambda () completed)))
             (ok (null (find-package :yeditor-dummy-pkg))))
        (when (member "yeditor-dummy-pkg" (installed-packages) :test #'equal)
          (uninstall-package "yeditor-dummy-pkg"))
        (uiop:delete-directory-tree root :validate t)))))

(deftest removal-cleans-packages-with-use-relationships
  (let* ((root (yeditor-components-tests::make-dummy-project))
         (file (merge-pathnames "yeditor-dummy-pkg/main.lisp" root)))
    (with-open-file (out file :direction :output :if-exists :append)
      (write-line "(defpackage :yeditor-dummy-client (:use :cl :yeditor-dummy-pkg))" out))
    (unwind-protect
         (progn
           (install-package "yeditor-dummy-pkg" :roots (list root))
           (ok (find-package :yeditor-dummy-client))
           (uninstall-package "yeditor-dummy-pkg")
           (ok (null (find-package :yeditor-dummy-client)))
           (ok (null (find-package :yeditor-dummy-pkg))))
      (when (member "yeditor-dummy-pkg" (installed-packages) :test #'equal)
        (uninstall-package "yeditor-dummy-pkg"))
      (uiop:delete-directory-tree root :validate t))))
