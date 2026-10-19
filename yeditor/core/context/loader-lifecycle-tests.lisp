;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context-loader-lifecycle-tests
  (:use :cl :rove :yeditor-context :yeditor-context/loader))
(in-package :yeditor-context-loader-lifecycle-tests)

(defmacro with-loader (&body body)
  `(let ((*component-registry* (make-hash-table :test #'eq))
         (*applied-components* (make-hash-table :test #'eq))
         (yeditor-context/reactivity::*coeffect-registry* (make-hash-table :test #'eq))
         (yeditor-context/reactivity::*current-context* (list :window-width 0)))
     ,@body))

(defun wait-until (predicate)
  (loop :repeat 200 :thereis (funcall predicate) :do (lem:receive-event 0.01)))

(defun temporary-config ()
  (merge-pathnames (format nil "t8-~A.lisp" (gensym)) (uiop:temporary-directory)))

(defun register-value-component (name setter &key fail)
  (register-component name :config-spec '(:value)
    :install (lambda (comp context)
               (let ((value (getf (getf context :config) :value)))
                 (when (and fail (eql value :fail)) (error "Rejected replacement"))
                 (list (yeditor-tx:install-effect comp :value
                         (lambda () (funcall setter value))
                         :undo-thunk (lambda () (funcall setter nil))))))))

(deftest explicit-config-and-component-names
  (with-loader
    (register-value-component 'configurable (lambda (v) (declare (ignore v))))
    (ok (equal (parse-config '((use-component configurable :config (:value 7))))
               '((configurable :value 7))))
    (ok (equal (parse-config '((use-component "configurable" :config (:value 7))))
               '((configurable :value 7))))
    (dolist (form '((use-component) (use-component nil) (use-component configurable :value)
                    (use-component configurable :config (:value 1 :value 2))
                    (use-component configurable :config (:value 1) :value 2)))
      (ok (signals (parse-config (list form)) 'error) "malformed declarations are rejected"))))

(deftest configuration-is-delivered-and-invalid-updates-preserve-active-state
  (with-loader
    (let ((value nil))
      (register-value-component 'configurable (lambda (v) (setf value v)) :fail t)
      (ok (null (apply-config '((configurable :value 1)))))
      (ok (eql value 1) "installer receives :config in its context")
      (let ((errors (apply-config '((configurable :bogus 2)))))
        (ok (= 1 (length errors)))
        (ok (search "BOGUS" (first errors) :test #'char-equal))
        (ok (eql value 1) "validation occurs before uninstall"))
      (ok (= 1 (length (apply-config '((configurable :value :fail))))))
      (ok (eql value 1) "failed replacement restores the previous configuration")
      (ok (equal (gethash 'configurable *applied-components*) '(:value 1)))
      (apply-config nil) (ok (null value)))))

(deftest borrowed-components-and-configured-reactivation
  (with-loader
    (let ((value nil) (borrowed-installs 0))
      (register-component 'borrowed :install (lambda (c ctx) (declare (ignore c ctx))
                                               (incf borrowed-installs) nil))
      (install-component 'borrowed)
      (apply-config '((borrowed))) (apply-config nil)
      (ok (= borrowed-installs 1))
      (ok (yeditor-context/reactivity:component-active-p 'borrowed)
          "removing a config line preserves a component installed outside the loader")
      (register-value-component 'conditional (lambda (v) (setf value v)))
      (yeditor-context/reactivity:define-coeffects 'conditional '(:remote-p t))
      (apply-config '((conditional :value 9)))
      (ok (null value) "configuration respects declared coeffects")
      (yeditor-context/reactivity:update-context :remote-p t)
      (ok (eql value 9))
      (yeditor-context/reactivity:update-context :remote-p nil)
      (yeditor-context/reactivity:update-context :remote-p t)
      (ok (eql value 9) "configuration survives conditional reinstall")
      (apply-config nil)
      (yeditor-context/reactivity:update-context :remote-p nil)
      (yeditor-context/reactivity:update-context :remote-p t)
      (ok (null value) "removed config cannot be resurrected by context events"))))

(deftest rapid-same-timestamp-changes-are-observed
  (let ((path (temporary-config)) (yeditor-context/loader::*config-file-write-date* nil))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output) (write-string "AAAA" out))
           (config-file-changed-p path)
           (let ((date (file-write-date path)))
             (with-open-file (out path :direction :output :if-exists :supersede) (write-string "BBBB" out))
             (ok (config-file-changed-p path) "same-size rewrites are detected even in one mtime tick")
             (ok (not (config-file-changed-p path)) "unchanged content does not trigger a reload")))
      (delete-file path))))

(deftest watcher-applies-50-real-rewrites-and-reports-errors
  (with-loader
    (lem-fake-interface:with-fake-interface ()
      (let ((path (temporary-config)) (values (make-array 4 :initial-element nil))
            (messages nil) (stable-installs 0))
        (dotimes (i 4)
          (let ((index i))
            (register-value-component (intern (format nil "WATCH-~D" i))
                                       (lambda (v) (setf (aref values index) v)))))
        (register-component 'stable :install (lambda (c ctx) (declare (ignore c ctx))
                                               (incf stable-installs) nil))
        (with-open-file (out path :direction :output) (write-string "" out))
        (unwind-protect
             (progn
               (start-config-watcher path :interval 0.005 :package *package*
                 :on-error (lambda (message) (push message messages)))
               (dotimes (step 50)
                 (let* ((mask (mod (* step 7) 16))
                        (expected (loop :for i :below 4
                                        :collect (when (logbitp i mask) (+ 10 i)))))
                   (with-open-file (out path :direction :output :if-exists :supersede)
                     (write-line "(use-component stable)" out)
                     (dotimes (i 4)
                       (when (nth i expected)
                         (format out "(use-component watch-~D :config (:value ~D))~%" i (nth i expected)))))
                   (ok (wait-until (lambda () (and (= 1 stable-installs) (equal (coerce values 'list) expected))))
                       "watcher applies the entire delta after every rewrite")
                   (ok (= stable-installs 1) "unchanged component is never reinstalled")))
               (with-open-file (out path :direction :output :if-exists :supersede)
                 (write-line "(use-component watch-0 :config (:invalid 1))" out))
               (ok (wait-until (lambda () messages)) "invalid options reach the UI error callback")
               (ok (search "INVALID" (first messages) :test #'char-equal))
               (ok (eql (aref values 0) 10) "invalid replacement preserves the last valid value")
               (delete-file path)
               (ok (wait-until (lambda () (every #'null values)))
                   "deleting the config removes its owned components"))
          (stop-config-watcher) (apply-config nil) (when (probe-file path) (delete-file path)))
        (ok (every #'null values) "all owned effects cleaned up")))))

(deftest config-overrides-rebase-retained-handles
  (with-loader
    (let ((value :baseline) (inner-installs 0))
      (dolist (name '(outer inner))
        (register-component name :config-spec '(:value)
          :install (lambda (comp ctx)
                     (when (eq comp 'inner) (incf inner-installs))
                     (let ((previous nil) (replacement (getf (getf ctx :config) :value)))
                       (list (yeditor-tx:install-effect comp :override
                               (lambda () (setf previous value value replacement))
                               :undo-thunk (lambda () (setf value previous))))))))
      (apply-config '((outer :value 1) (inner :value 2)))
      (ok (eql value 2))
      (ok (< (getf (gethash 'outer *component-registry*) :activation-order)
             (getf (gethash 'inner *component-registry*) :activation-order))
          "activation order persists in the registry")
      (let ((handles (getf (gethash 'inner *component-registry*) :state)))
        (ok (null (apply-config '((outer :value 3) (inner :value 2)))))
        (ok (eql value 2))
        (ok (= 1 inner-installs))
        (ok (eq handles (getf (gethash 'inner *component-registry*) :state))))
      (apply-config '((inner :value 2)))
      (ok (eql value 2))
      (apply-config nil)
      (ok (eq value :baseline) "removing the final layer restores the original value"))))

(deftest standard-reader-only-and-no-evaluation
  (let ((path (temporary-config)) (*readtable* (copy-readtable nil)) (called nil))
    (set-macro-character #\! (lambda (stream char) (declare (ignore stream char)) (setf called t)))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output) (write-string "!" out))
           (read-config-forms path)
           (ok (not called) "custom reader macros are not used")
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string "#.(error \"reader evaluation\")" out))
           (ok (signals (read-config-forms path) 'error)))
      (delete-file path))))

(deftest stopping-watcher-invalidates-queued-snapshots
  (with-loader
    (let ((path (temporary-config)) (value nil))
      (register-value-component 'queued (lambda (v) (setf value v)))
      (with-open-file (out path :direction :output)
        (write-line "(use-component queued :config (:value 8))" out))
      (unwind-protect
           (progn
             (start-config-watcher path :interval 0.005 :package *package*)
             (sleep 0.05)
             (stop-config-watcher)
             (lem:receive-event 0.02)
             (ok (null value))
             (ok (zerop (hash-table-count *applied-components*))))
        (stop-config-watcher) (apply-config nil) (delete-file path)))))

(deftest configured-component-100-install-remove-cycles
  (with-loader
    (let ((value nil))
      (register-value-component 'repeatable (lambda (v) (setf value v)))
      (dotimes (i 100)
        (ok (null (apply-config (list (list 'repeatable :value i)))))
        (ok (eql value i))
        (ok (null (apply-config nil)))
        (ok (null value))
        (ok (zerop (hash-table-count *applied-components*)))
        (ok (not (yeditor-context/reactivity:component-active-p 'repeatable)))))))
