;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-forge-tests
  (:use :cl :rove :yeditor-context :yeditor-enterprise/forge))
(in-package :yeditor-enterprise-forge-tests)

(defclass canned (yeditor-components/models:provider) ((reply :initarg :reply :reader reply))
  (:default-initargs :name "canned" :base-url "http://127.0.0.1" :default-model "canned"))

(defmethod yeditor-components/models:provider-generate ((p canned) prompt &key model system on-chunk)
  (declare (ignore prompt model system on-chunk))
  (reply p))

(defun temp-dir ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "yeditor-forge-~D/" (random 1000000)) (uiop:default-temporary-directory))))

(defun forge (source &key (directory (temp-dir)))
  (let ((*forge-directory* directory))
    (forge-component "a request" (make-instance 'canned :reply source))))

(defparameter *valid*
  "(define-component forge-demo :version \"0.1.0\"
     :install (lambda (comp ctx) (declare (ignore ctx))
                (list (install-keybinding comp *global-keymap* \"C-c z z\" 'forward-char)
                      (install-variable comp '*forge-demo-flag* :on)
                      (install-hook comp '*find-file-hook* (lambda (&rest args) (declare (ignore args)) nil)))))")

(deftest test-forge-promotes-a-reversible-component
  (lem-fake-interface:with-fake-interface ()
    (let* ((directory (temp-dir))
           (before (state-fingerprint))
           (result (forge (format nil "```lisp~%~A~%```" *valid*) :directory directory)))
      (unwind-protect
           (progn
             (ok (eq (getf result :status) :promoted) (format nil "promoted (~A)" (getf result :reason)))
             (ok (probe-file (getf result :path)) "source saved under the forge directory")
             (ok (search "forge-demo" (uiop:read-file-string (getf result :path))) "saved without the code fence")
             (ok (gethash (getf result :name) *component-registry*) "registered as a component")
             (ok (equal (state-fingerprint) before) "editor state identical after the reversibility rounds")
             (install-component (getf result :name))
             (ok (eq (let ((p (lem:keymap-find lem:*global-keymap* (lem:parse-keyspec "C-c z z"))))
                       (and p (lem:prefix-suffix p)))
                     'lem:forward-char)
                 "the promoted component works when installed")
             (uninstall-component (getf result :name)))
        (unregister-component (getf result :name))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defparameter *rejected-sources*
  (list (cons "without an inverse"
              "(define-component bad-a :install (lambda (comp ctx) (list (yeditor-tx:install-effect comp :x (lambda () nil)))))")
        (cons "not allowed"
              "(define-component bad-b :install (lambda (comp ctx) (delete-file \"/tmp/x\") (list)))")
        (cons "not allowed"
              "(define-component bad-c :install (lambda (comp ctx) (list (install-process comp (list \"rm\" \"-rf\" \"/\")))))")
        (cons "needs a lambda"
              "(define-component bad-d :install (lambda (comp ctx) (list (install-hook comp '*find-file-hook* 'delete-file))))")
        (cons "literals"
              "(define-component bad-e :install (lambda (comp ctx) (list (install-variable comp '*find-file-hook* '((delete-file . 0))))))")
        (cons "unreadable"
              "(define-component bad-f :install #.(delete-file \"/tmp/x\"))")
        (cons "exactly one form"
              "(define-component bad-g :install (lambda (comp ctx) (list))) (delete-file \"/tmp/x\")")
        (cons "must be (define-component"
              "(defun bad-h () (delete-file \"/tmp/x\"))")))

(deftest test-forge-rejects-unsafe-components
  (lem-fake-interface:with-fake-interface ()
    (loop :for (expected . source) :in *rejected-sources*
          :for directory := (temp-dir)
          :do (let ((result (forge source :directory directory)))
                (ok (eq (getf result :status) :discarded) (format nil "discarded: ~A" expected))
                (ok (search expected (or (getf result :reason) "")) (format nil "reason mentions ~S: ~A" expected (getf result :reason)))
                (ok (not (uiop:directory-exists-p directory)) "nothing saved")
                (ok (not (and (getf result :name) (gethash (getf result :name) *component-registry*)))
                    "nothing left registered")))
    (let ((*max-component-bytes* 20))
      (ok (search "limit" (getf (forge *valid*) :reason)) "oversized source rejected"))))

(defvar *leaked* nil)

(define-component irreversible-probe
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (yeditor-tx:install-effect comp :x
                                              (lambda () (push (lem:make-buffer (format nil "leak-~D" (random 100000))) *leaked*))
                                              :undo-thunk (lambda () nil)))))

(deftest test-reversibility-check-detects-leaks
  (lem-fake-interface:with-fake-interface ()
    (ok (not (yeditor-enterprise/forge::reversible-p 'irreversible-probe))
        "a component whose undo leaves a buffer behind fails the reversibility test")
    (mapc #'lem:kill-buffer *leaked*)
    (unregister-component 'irreversible-probe)))
