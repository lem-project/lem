;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/packages
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/palette #:*palette-actions*)
  (:export :le-package-onboarding
           :package-roots
           :*package-roots*
           :scan-local-systems
           :install-package
           :uninstall-package
           :installed-packages
           :package-component-name
           :*irreversibility-notice*
           :yeditor-install-package
           :yeditor-uninstall-package))
(in-package :yeditor-components/packages)

(defparameter *package-roots*
  (list (merge-pathnames "common-lisp/" (user-homedir-pathname))
        (merge-pathnames "quicklisp/local-projects/" (user-homedir-pathname)))
  "Directories scanned for local ASDF systems (plan T14).")

(defparameter *irreversibility-notice*
  "Loading Lisp code is only partly reversible: removal cleans up newly created
packages and newly loaded ASDF systems, but changes to existing definitions,
global state or external files can remain. A restart may be needed. Proceed?")

(defvar *installed* nil "List of system names installed through install-package.")

(defvar *package-job* nil "Token of the current asynchronous package UI operation.")

(defun installed-packages ()
  "Return the systems installed through package onboarding."
  (reverse *installed*))

(defun package-roots ()
  "Return configured roots plus the active Quicklisp installation's local projects."
  (let ((symbol (and (find-package "QL") (find-symbol "*LOCAL-PROJECT-DIRECTORIES*" "QL"))))
    (remove-duplicates (append *package-roots* (when (and symbol (boundp symbol)) (symbol-value symbol)))
                       :test #'equal)))

(defun scan-local-systems (&optional (roots (package-roots)))
  "Alist (system-name . asd-pathname) of the .asd files found under ROOTS."
  (let ((found nil))
    (dolist (root roots (sort found (function string<) :key (function car)))
      (when (uiop:directory-exists-p root)
        (dolist (asd (directory (merge-pathnames "**/*.asd" root)))
          (pushnew (cons (pathname-name asd) asd) found :key (function car) :test (function string=)))))))

(defun package-component-name (system)
  "Return the stable component identifier for the onboarded SYSTEM."
  (intern (format nil "PACKAGE/~A" (string-upcase system)) :yeditor-components/packages))

(defun install-effect* (comp kind fn undo)
  (yeditor-tx:install-effect comp kind fn :undo-thunk undo))

(defun install-source-directory (comp directory)
  (let ((owned nil))
    (install-effect* comp :source-registry
                     (lambda ()
                       (setf owned (not (member directory asdf:*central-registry* :test #'equal)))
                       (when owned (push directory asdf:*central-registry*)))
                     (lambda ()
                       (when owned
                         (setf asdf:*central-registry*
                               (remove directory asdf:*central-registry* :test #'equal :count 1)))))))

(defun install-system-load (comp system)
  "Load SYSTEM and clean up acquired packages/systems on failure or undo."
  (let ((created-packages nil) (created-systems nil))
    (labels ((cleanup ()
               (dolist (name created-systems) (asdf:clear-system name))
               ;; Package enumeration is unordered. Break use relationships
               ;; before deletion so SBCL never asks for an interactive restart.
               (dolist (package created-packages)
                 (dolist (user (package-used-by-list package))
                   (unuse-package package user)))
               (dolist (package created-packages)
                 (when (package-name package) (delete-package package)))
               (setf created-packages nil created-systems nil)))
      (install-effect*
       comp :lisp-load
       (lambda ()
         (let ((packages-before (list-all-packages))
               (registered-before (asdf:registered-systems))
               (loaded-before (asdf:already-loaded-systems))
               (loaded nil))
           (unwind-protect
                (progn
                  ;; Quicklisp is optional; use its public function only when loaded.
                  (if (find-package "QL")
                      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
                      (asdf:load-system system))
                  (setf loaded t))
             (setf created-packages (set-difference (list-all-packages) packages-before)
                   created-systems
                   (union (set-difference (asdf:registered-systems) registered-before :test #'equal)
                          (set-difference (asdf:already-loaded-systems) loaded-before :test #'equal)
                          :test #'equal))
             ;; A failing apply is not added to the transaction's executed list.
             ;; It must undo its own partial load before earlier effects roll back.
             (unless loaded (cleanup)))))
       #'cleanup))))

(defun install-package (system &key (roots (package-roots)) (confirm (constantly t)))
  "Install the local ASDF SYSTEM found under ROOTS as a component (PACKAGE/<NAME>),
   so the install and its uninstall go through the transaction log. CONFIRM is called
   with the system name and *irreversibility-notice*; a false result cancels.
   Returns the component name, or nil when cancelled."
  (setf system (string-downcase system))
  (when (member system *installed* :test #'equal)
    (let ((name (package-component-name system)))
      (install-component name)
      (return-from install-package name)))
  (let ((asd (cdr (assoc system (scan-local-systems roots) :test (function string-equal)))))
    (unless (or asd (find-package "QL"))
      (editor-error "System ~A not found under ~{~A~^, ~}" system roots))
    (unless (funcall confirm system *irreversibility-notice*)
      (return-from install-package nil))
    (let ((name (package-component-name system))
          (directory (and asd (uiop:pathname-directory-pathname asd))))
      (register-component name
                          :version "local"
                          :install (lambda (comp ctx)
                                     (declare (ignore ctx))
                                     (append (when directory (list (install-source-directory comp directory)))
                                             (list (install-system-load comp system)))))
      (handler-case
          (install-component name)
        (error (c)
          (unregister-component name)
          ;; ASDF signals missing-component; Quicklisp (optional, so not referenced
          ;; directly) signals its own system-not-found.
          (if (or (typep c (quote asdf:missing-component))
                  (string= (symbol-name (type-of c)) "SYSTEM-NOT-FOUND"))
              (editor-error "System ~A could not be found" system)
              (error c))))
      (pushnew system *installed* :test (function string-equal))
      name)))

(defun uninstall-package (system)
  "Uninstall SYSTEM installed by install-package and drop its component."
  (setf system (string-downcase system))
  (let ((name (package-component-name system)))
    (uninstall-component name)
    (unregister-component name)
    (setf *installed* (remove system *installed* :test (function string-equal)))
    t))

(defun run-package-task (function success)
  (when *package-job* (editor-error "A package operation is already running"))
  (let ((token (gensym "PACKAGE-JOB")))
    (setf *package-job* token)
    (handler-case
        (bt2:make-thread
         (lambda ()
           (handler-case
               (let ((result (funcall function)))
                 (send-event (lambda () (setf *package-job* nil) (funcall success result))))
             (error (condition)
               (send-event (lambda ()
                             (setf *package-job* nil)
                             (message "Package operation failed: ~A" condition))))))
         :name "yeditor-package-onboarding")
      (error (condition) (setf *package-job* nil) (error condition)))))

(define-command yeditor-install-package () ()
  "Choose a local system, confirm partial reversibility, then load it asynchronously."
  (let ((roots (package-roots)))
    (run-package-task
     (lambda () (mapcar #'car (scan-local-systems roots)))
     (lambda (names)
       (unless names (editor-error "No local packages found"))
       (let ((system (prompt-for-string "Install package: "
                                        :completion-function (lambda (s) (completion s names))
                                        :test-function (lambda (s) (member s names :test #'equal)))))
         (when (prompt-for-y-or-n-p *irreversibility-notice*)
           (message "Installing ~A..." system)
           (run-package-task
            (lambda () (install-package system :roots roots))
            (lambda (component) (declare (ignore component)) (message "Installed ~A" system)))))))))

(define-command yeditor-uninstall-package () ()
  "Remove an onboarded system asynchronously; existing-definition changes may remain."
  (let ((names (installed-packages)))
    (unless names (editor-error "No onboarded packages to remove"))
    (let ((system (prompt-for-string "Remove package: "
                                     :completion-function (lambda (s) (completion s names))
                                     :test-function (lambda (s) (member s names :test #'equal)))))
      (run-package-task (lambda () (uninstall-package system))
                        (lambda (result) (declare (ignore result)) (message "Removed ~A" system))))))

(define-component le-package-onboarding
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*palette-actions*
                                     (append (list (cons "Install package..." #'yeditor-install-package)
                                                   (cons "Remove package..." #'yeditor-uninstall-package))
                                             *palette-actions*)))))
