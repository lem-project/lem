;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-durability-crash-child (:use :cl))
(in-package :yeditor-durability-crash-child)

;;; Standalone process fixture. Bootstrap before reading qualified library symbols.
(load (merge-pathnames "../../../.qlot/setup.lisp" *load-truename*))
(dolist (file '("../tx/yeditor-tx.asd" "yeditor-context.asd" "yeditor-context-durability.asd"))
  (asdf:load-asd (merge-pathnames file *load-truename*)))
(asdf:load-system :yeditor-context-durability)

(defvar *crashy-flag* nil)
(defvar *saved-flag* :baseline)
(defvar *ready-path* nil)

(yeditor-context:define-component saved :persist t
  :install (lambda (component context)
             (declare (ignore context))
             (let ((before *saved-flag*))
               (list (yeditor-tx:install-effect
                      component :variable (lambda () (setf *saved-flag* :restored))
                      :undo-thunk (lambda () (setf *saved-flag* before)))))))

(yeditor-context:define-component crashy :persist t
  :install (lambda (component context)
             (declare (ignore context))
             (list (yeditor-tx:install-effect
                    component :variable (lambda () (setf *crashy-flag* t))
                    :undo-thunk (lambda () (setf *crashy-flag* nil)))))
  :activate (lambda (component)
              (declare (ignore component))
              (assert *crashy-flag*)
              (with-open-file (out *ready-path* :direction :output :if-exists :supersede)
                (write-line "effects-applied" out))
              (loop :do (sleep 0.01))))

(destructuring-bind (mode path ready) (uiop:command-line-arguments)
  (setf *ready-path* ready)
  (if (equal mode "recover")
      (let ((result (yeditor-context/durability:recover :path path)))
        (assert (equal (getf result :cancelled) '("YEDITOR-DURABILITY-CRASH-CHILD::CRASHY")))
        (assert (equal (getf result :reinstalled) '("YEDITOR-DURABILITY-CRASH-CHILD::SAVED")))
        (assert (null *crashy-flag*))
        (assert (eq :inactive (getf (gethash 'crashy yeditor-context:*component-registry*) :status)))
        (assert (eq *saved-flag* :restored))
        (assert (null (getf result :errors)))
        (multiple-value-bind (live pending) (yeditor-context/durability:replay-log :path path)
          (assert (equal live '("YEDITOR-DURABILITY-CRASH-CHILD::SAVED")))
          (assert (null pending)))
        (yeditor-context:uninstall-component 'saved)
        (assert (eq *saved-flag* :baseline)))
      (progn
        (yeditor-context/durability:enable-durability :path path)
        (yeditor-context:install-component 'saved)
        (yeditor-context:install-component 'crashy))))
