;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context-durability-tests
  (:use :cl :rove :yeditor-context :yeditor-context/durability))
(in-package :yeditor-context-durability-tests)

(defun tmp-log (name)
  (let ((p (merge-pathnames (format nil "yeditor-t9-~A-~A.log" name (random 1000000))
                            (uiop:default-temporary-directory))))
    (when (probe-file p) (delete-file p))
    p))

(defun read-events (path)
  (with-open-file (in path)
    (loop :for line := (read-line in nil) :while line
          :collect (gethash "event" (yason:parse line)))))

(defvar *durable-flag* nil)

(define-component durable-comp :version "1" :persist t
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (yeditor-tx:install-effect comp :noop
                                              (lambda () (setf *durable-flag* t))
                                              :undo-thunk (lambda () (setf *durable-flag* nil))))))

(define-component failing-commit-comp :version "1"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (yeditor-tx:install-effect comp :noop (lambda () (error "commit boom"))
                                              :undo-thunk (lambda () nil)))))

(deftest test-lifecycle-events-logged
  (let ((path (tmp-log "events")))
    (enable-durability :path path)
    (unwind-protect
         (progn
           (install-component 'durable-comp)
           (toggle-component 'durable-comp)
           (toggle-component 'durable-comp)
           (uninstall-component 'durable-comp)
           (ignore-errors (install-component 'failing-commit-comp)))
      (disable-durability))
    (ok (equal (read-events path)
               '("install-begin" "install-commit" "suspend" "resume" "uninstall"
                 "install-begin" "install-abort"))
        "every transition appended in order")
    (ok (null (getf (gethash 'failing-commit-comp *component-registry*) :state))
        "failed commit leaves the component not installed")
    (delete-file path)))

(deftest test-replay-classification
  (let ((path (tmp-log "replay")))
    (log-event :install-begin "P::A" :path path)
    (log-event :install-commit "P::A" :path path)
    (log-event :install-begin "P::B" :path path)
    (log-event :install-commit "P::B" :path path)
    (log-event :uninstall "P::B" :path path)
    (log-event :install-begin "P::C" :path path)
    (log-event :install-begin "P::D" :path path)
    (log-event :install-abort "P::D" :path path)
    (with-open-file (out path :direction :output :if-exists :append)
      (write-string "{\"ts\":1,\"event\":\"install-com" out))
    (multiple-value-bind (live cancelled) (replay-log :path path)
      (ok (equal live '("P::A")) "only committed, not uninstalled, is live")
      (ok (equal cancelled '("P::C")) "begin without commit/abort is cancelled; torn line skipped"))
    (delete-file path)))

(deftest test-crash-mid-install-recovery
  ;; a separate SBCL dies (exit :abort) inside the install function
  (let* ((path (tmp-log "crash"))
         (root (namestring (uiop:getcwd)))
         (script (merge-pathnames "yeditor-t9-crash.lisp" (uiop:default-temporary-directory))))
    (with-open-file (out script :direction :output :if-exists :supersede)
      (format out "(load ~S)~%" (concatenate 'string root ".qlot/setup.lisp"))
      (dolist (asd '("core/tx/yeditor-tx.asd" "core/context/yeditor-context.asd"
                     "core/context/yeditor-context-durability.asd"))
        (format out "(asdf:load-asd ~S)~%" (concatenate 'string root "yeditor/" asd)))
      (format out "(asdf:load-system :yeditor-context-durability)~%")
      (format out "(yeditor-context:define-component crashy :install (lambda (c x) (declare (ignore c x)) (sb-ext:exit :code 7 :abort t)))~%")
      (format out "(yeditor-context/durability:enable-durability :path ~S)~%" (namestring path))
      (format out "(yeditor-context:install-component 'crashy)~%"))
    (let ((code (nth-value 2 (uiop:run-program (list "sbcl" "--script" (namestring script))
                                               :ignore-error-status t
                                               :output nil :error-output nil))))
      (ok (= code 7) "child process crashed inside install"))
    (ok (equal (read-events path) '("install-begin")) "log holds the begin, no commit")
    (let ((result (recover :path path)))
      (ok (equal (getf result :cancelled) '("COMMON-LISP-USER::CRASHY"))
          "recovery reports the interrupted install as cancelled")
      (ok (null (getf result :reinstalled)) "nothing re-installed"))
    (ok (equal (read-events path) '("install-begin" "install-cancelled"))
        "cancellation recorded for audit")
    (multiple-value-bind (live cancelled) (replay-log :path path)
      (ok (and (null live) (null cancelled)) "second boot: clean state"))
    (delete-file path)
    (delete-file script)))

(deftest test-persist-component-reinstalled
  (let ((path (tmp-log "persist")))
    (uninstall-component 'durable-comp)
    (setf *durable-flag* nil)
    (log-event :install-begin 'durable-comp :path path)
    (log-event :install-commit 'durable-comp :path path)
    (let ((result (recover :path path)))
      (ok (equal (getf result :reinstalled)
                 (list (format nil "~A::DURABLE-COMP" (package-name *package*))))
          ":persist t component re-installed on boot")
      (ok *durable-flag* "its effects are applied"))
    (uninstall-component 'durable-comp)
    (delete-file path)))

(deftest test-replay-10k-under-100ms
  (let ((path (tmp-log "bench")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (dotimes (i 10000)
        (format out "{\"ts\":~D,\"event\":\"~A\",\"component\":\"P::C~D\"}~%"
                i (if (evenp i) "install-begin" "install-commit") (floor i 2))))
    (let ((times (loop :repeat 3
                       :collect (let ((t0 (get-internal-real-time)))
                                  (replay-log :path path)
                                  (/ (* 1000.0 (- (get-internal-real-time) t0))
                                     internal-time-units-per-second)))))
      (format t "~&replay 10k entries (ms): ~{~,1F~^ ~}~%" times)
      (ok (< (second (sort (copy-list times) #'<)) 100) "median replay of 10k entries < 100 ms"))
    (delete-file path)))
