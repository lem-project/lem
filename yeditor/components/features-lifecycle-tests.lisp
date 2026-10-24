;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-features-lifecycle-tests
  (:use :cl :rove :lem :yeditor-context)
  (:import-from :yeditor-components/features
                #:le-git #:le-project #:le-tree #:le-inherited-entry-points-off
                #:*feature-keys*))
(in-package :yeditor-features-lifecycle-tests)

(defun binding (key)
  (let ((prefix (keymap-find *global-keymap* (parse-keyspec key))))
    (and prefix (prefix-suffix prefix))))

(defun keymap-snapshot (map)
  (list map
        (mapcar (lambda (prefix)
                  (list prefix (prefix-key prefix)
                        (let ((suffix (prefix-suffix prefix)))
                          (if (typep suffix 'keymap) (keymap-snapshot suffix) suffix))))
                (keymap-prefixes map))))

(defun call-with-feature-fixture (function)
  (let ((*global-keymap* (make-keymap))
        (*switch-to-buffer-hook* nil))
    (dolist (feature *feature-keys*)
      (dolist (key (cdr feature)) (define-key *global-keymap* (car key) (cdr key))))
    (define-key *global-keymap* "C-x p z" 'next-line)
    (add-hook *switch-to-buffer-hook* 'next-line 17)
    (add-hook *switch-to-buffer-hook* 'lem/filer:sync-filer-to-buffer-directory 17)
    (add-hook *switch-to-buffer-hook* 'previous-line 17)
    (unwind-protect (funcall function)
      (dolist (component '(le-tree le-project le-git le-inherited-entry-points-off))
        (uninstall-component component)))))

(deftest exact-global-state-after-100-feature-cycles
  (call-with-feature-fixture
   (lambda ()
     (let ((keys (keymap-snapshot *global-keymap*))
           (hooks (copy-tree *switch-to-buffer-hook*))
           (buffers (copy-list (buffer-list))))
       (dotimes (cycle 100)
         (install-component 'le-inherited-entry-points-off)
         (let ((basal (keymap-snapshot *global-keymap*)))
           (dolist (component '(le-git le-project le-tree))
             (install-component component)
             (toggle-component component)
             (toggle-component component)
             (uninstall-component component)
             (ok (equal basal (keymap-snapshot *global-keymap*)) "feature restores the entire prefix structure")
             (ok (eq 'next-line (binding "C-x p z")) "unrelated sibling survives")))
         (uninstall-component 'le-inherited-entry-points-off)
         (ok (equal keys (keymap-snapshot *global-keymap*)) "basal removal restores original prefix objects and ordering")
         (ok (equal hooks *switch-to-buffer-hook*) "hook weights and ordering restored exactly")
         (ok (equal buffers (buffer-list)) "no buffers or permanent UI are created by feature activation"))))))

(deftest basal-removal-preserves-later-user-bindings-and-hooks
  (call-with-feature-fixture
   (lambda ()
     (install-component 'le-inherited-entry-points-off)
     (define-key *global-keymap* "C-x g" 'previous-line)
     (add-hook *switch-to-buffer-hook* 'lem/filer:sync-filer-to-buffer-directory 83)
     (uninstall-component 'le-inherited-entry-points-off)
     (ok (eq 'previous-line (binding "C-x g")) "a user's replacement is not overwritten")
     (ok (= 83 (cdr (assoc 'lem/filer:sync-filer-to-buffer-directory *switch-to-buffer-hook*)))
         "a user's replacement hook weight is not overwritten"))))

(deftest independent-features-do-not-remove-later-prefix-siblings
  (let ((*global-keymap* (make-keymap)) (*switch-to-buffer-hook* nil))
    (unwind-protect
         (progn
           (install-component 'le-git)
           (install-component 'le-project)
           (install-component 'le-tree)
           (define-key *global-keymap* "C-x p z" 'next-line)
           (uninstall-component 'le-git)
           (ok (eq 'lem-core/commands/project:project-find-file (binding "C-x p f"))
               "removing Git preserves the independently active project feature")
           (ok (eq 'lem/filer:filer (binding "C-x d")))
           (uninstall-component 'le-project)
           (ok (eq 'next-line (binding "C-x p z")) "new user siblings survive created-prefix cleanup")
           (ok (eq 'lem/filer:filer (binding "C-x d"))))
      (dolist (component '(le-tree le-project le-git)) (uninstall-component component)))))

(deftest custom-prefix-and-hook-priorities-survive-cleanup
  (let ((*global-keymap* (make-keymap)) (*switch-to-buffer-hook* nil)
        (custom (make-keymap :undef-hook 'next-line)))
    (unwind-protect
         (progn
           (install-component 'le-git)
           (define-key *global-keymap* "C-x" custom)
           (uninstall-component 'le-git)
           (ok (eq custom (binding "C-x")) "a user prefix with a fallback is not considered empty"))
      (uninstall-component 'le-git)))
  (call-with-feature-fixture
   (lambda ()
     (install-component 'le-inherited-entry-points-off)
     (add-hook *switch-to-buffer-hook* 'forward-char 100)
     (add-hook *switch-to-buffer-hook* 'backward-char -1)
     (uninstall-component 'le-inherited-entry-points-off)
     (ok (equal '(100 17 17 17 -1) (mapcar #'cdr *switch-to-buffer-hook*)))
     (ok (eq 'forward-char (caar *switch-to-buffer-hook*)))
     (ok (eq 'backward-char (car (car (last *switch-to-buffer-hook*))))))))
