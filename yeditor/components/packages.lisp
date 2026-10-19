;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/packages
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :*package-roots*
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
  "Loading Lisp code is only partly reversible: uninstalling deletes the packages and
ASDF systems this load created, but definitions it made inside packages that already
existed (methods, CL-USER functions, global state) remain until restart. Proceed?")

(defvar *installed* nil "List of system names installed through install-package.")

(defun installed-packages () (reverse *installed*))

(defun scan-local-systems (&optional (roots *package-roots*))
  "Alist (system-name . asd-pathname) of the .asd files found under ROOTS."
  (let ((found nil))
    (dolist (root roots (sort found (function string<) :key (function car)))
      (when (uiop:directory-exists-p root)
        (dolist (asd (directory (merge-pathnames "**/*.asd" root)))
          (pushnew (cons (pathname-name asd) asd) found :key (function car) :test (function string=)))))))

(defun package-component-name (system)
  (intern (format nil "PACKAGE/~A" (string-upcase system)) :yeditor-components/packages))

(defun install-effect* (comp kind fn undo)
  (yeditor-tx:install-effect comp kind fn :undo-thunk undo))

(defun install-source-directory (comp directory)
  (install-effect* comp :source-registry
                   (lambda () (push directory asdf:*central-registry*))
                   (lambda () (setf asdf:*central-registry*
                                    (remove directory asdf:*central-registry* :test (function equal))))))

(defun install-system-load (comp system)
  "Load SYSTEM; the inverse deletes the packages and clears the ASDF systems the load created."
  (let ((created-packages nil) (created-systems nil))
    (install-effect* comp :lisp-load
                     (lambda ()
                       (let ((packages-before (list-all-packages))
                             (systems-before (asdf:already-loaded-systems)))
                         (if (find-package "QL")
                             (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
                             (asdf:load-system system))
                         (setf created-packages (set-difference (list-all-packages) packages-before)
                               created-systems (set-difference (asdf:already-loaded-systems) systems-before
                                                               :test (function string=)))))
                     (lambda ()
                       (dolist (name created-systems) (asdf:clear-system name))
                       (dolist (package created-packages)
                         (when (package-name package) (ignore-errors (delete-package package))))
                       (setf created-packages nil created-systems nil)))))

(defun install-package (system &key (roots *package-roots*) (confirm (constantly t)))
  "Install the local ASDF SYSTEM found under ROOTS as a component (PACKAGE/<NAME>),
   so the install and its uninstall go through the transaction log. CONFIRM is called
   with the system name and *irreversibility-notice*; a false result cancels.
   Returns the component name, or nil when cancelled."
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
          (handler-bind ((error (lambda (c) (declare (ignore c)) (unregister-component name))))
            (install-component name))
        (error (c)
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
  (let ((name (package-component-name system)))
    (uninstall-component name)
    (unregister-component name)
    (setf *installed* (remove system *installed* :test (function string-equal)))
    t))

(define-command yeditor-install-package () ()
  "Install a local Common Lisp system (from ~/common-lisp or Quicklisp local-projects)."
  (let* ((names (mapcar (function car) (scan-local-systems)))
         (system (prompt-for-string "Install package: "
                                    :completion-function (lambda (s) (completion s names))
                                    :test-function (lambda (s) (member s names :test (function string=))))))
    (when (install-package system :confirm (lambda (name notice)
                                             (declare (ignore name))
                                             (prompt-for-y-or-n-p notice)))
      (message "Installed ~A (M-x yeditor-uninstall-package to remove)" system))))

(define-command yeditor-uninstall-package () ()
  "Uninstall a package installed with yeditor-install-package."
  (let* ((names (installed-packages))
         (system (prompt-for-string "Uninstall package: "
                                    :completion-function (lambda (s) (completion s names))
                                    :test-function (lambda (s) (member s names :test (function string=))))))
    (uninstall-package system)
    (message "Uninstalled ~A" system)))
