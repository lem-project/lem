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
  ;; SIGKILL after effects have applied, before install-commit, then a fresh SBCL boot.
  (let* ((path (tmp-log "crash"))
         (ready (tmp-log "ready"))
         (script (merge-pathnames "durability-crash-child.lisp"
                                  (asdf:system-source-directory :yeditor-context-durability)))
         (diagnostics (tmp-log "child-output"))
         (process nil))
    (unwind-protect
         (progn
           (setf process (uiop:launch-program (list "sbcl" "--script" (namestring script)
                                                   "crash" (namestring path) (namestring ready))
                                              :output diagnostics :error-output :output))
           (let ((deadline (+ (get-internal-real-time) (* 30 internal-time-units-per-second))))
             (loop :until (or (probe-file ready) (not (uiop:process-alive-p process))
                              (> (get-internal-real-time) deadline))
                   :do (sleep 0.01)))
           (ok (probe-file ready) "child reached activation with applied effects and captured inverse")
           (uiop:terminate-process process :urgent t)
           (ok (not (zerop (uiop:wait-process process))) "external kill interrupts the install")
           (ok (equal (read-events path) '("install-begin" "install-commit" "install-begin"))
               "only the completed component has a commit")
           (let ((code (nth-value 2 (uiop:run-program
                                    (list "sbcl" "--script" (namestring script) "recover"
                                          (namestring path) (namestring ready))
                                    :ignore-error-status t :output diagnostics
                                    :if-output-exists :append :error-output :output))))
             (ok (= code 0) "fresh boot cancels pending install and restores committed component with a fresh inverse"))
           (ok (equal (read-events path) '("install-begin" "install-commit" "install-begin"
                                        "install-cancelled" "install-begin" "install-commit"))
               "cancellation is durably auditable on the next boot"))
      (when (and process (uiop:process-alive-p process))
        (uiop:terminate-process process :urgent t) (uiop:wait-process process))
      (dolist (file (list path ready diagnostics))
        (when (probe-file file) (delete-file file))))))

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
                                  (let ((result (recover :path path)))
                                    (assert (= 5000 (length (getf result :skipped)))))
                                  (/ (* 1000.0 (- (get-internal-real-time) t0))
                                     internal-time-units-per-second)))))
      (format t "~&boot reconciliation of 10k entries (ms): ~{~,1F~^ ~}~%" times)
      (ok (< (second (sort (copy-list times) #'<)) 100) "median boot replay + reconciliation of 10k entries < 100 ms"))
    (delete-file path)))

(deftest test-torn-tail-remains-recoverable-after-append
  (let ((path (tmp-log "torn")))
    (unwind-protect
         (progn
           (log-event :install-begin "P::PENDING" :path path)
           (with-open-file (out path :direction :output :if-exists :append)
             (write-string "{\"event\":\"install-com" out))
           (recover :path path)
           (multiple-value-bind (live cancelled) (replay-log :path path)
             (ok (null live))
             (ok (null cancelled) "cancellation survives a second boot after a torn write")))
      (delete-file path))))

(deftest test-replay-uses-last-activation-order
  (let ((path (tmp-log "order")))
    (unwind-protect
         (progn
           (dolist (id '("P::A" "P::B"))
             (log-event :install-begin id :path path)
             (log-event :install-commit id :path path))
           (log-event :suspend "P::A" :path path)
           (log-event :resume "P::A" :path path)
           (ok (equal (replay-log :path path) '("P::B" "P::A"))
               "recovery rebuilds override layers in their latest activation order"))
      (delete-file path))))

(deftest test-invalid-records-cannot-overwrite-a-commit
  (let ((path (tmp-log "invalid")))
    (unwind-protect
         (progn
           (log-event :install-commit "P::A" :path path)
           (with-open-file (out path :direction :output :if-exists :append)
             (format out "{\"event\":\"surprise\",\"component\":\"P::A\"}~%{\"event\":null}~%"))
           (ok (equal (replay-log :path path) '("P::A"))
               "malformed or unknown events cannot erase valid committed state"))
      (delete-file path))))

(deftest test-durability-hook-ownership
  (let* ((path (tmp-log "hooks"))
         (seen nil)
         (prior (lambda (event component) (push (list event component) seen)))
         (*lifecycle-hook* prior))
    (unwind-protect
         (progn
           (enable-durability :path path)
           (enable-durability :path path)
           (funcall *lifecycle-hook* :install-begin 'durable-comp)
           (ok (= 1 (length seen)) "prior lifecycle observer runs once")
           (ok (equal (read-events path) '("install-begin")) "enabling twice never duplicates logging")
           (disable-durability)
           (ok (eq *lifecycle-hook* prior) "disabling restores the borrowed observer")
           (enable-durability :path path)
           (setf *lifecycle-hook* prior)
           (disable-durability)
           (ok (eq *lifecycle-hook* prior) "disable preserves a replacement observer"))
      (disable-durability)
      (when (probe-file path) (delete-file path)))))

(deftest test-recovery-respects-session-status-and-journals-failure
  (let ((path (tmp-log "status"))
        (*component-registry* (make-hash-table :test #'eq))
        (*lifecycle-hook* nil)
        (installs 0))
    (register-component 'empty :persist t :install (lambda (c x) (declare (ignore c x)) (incf installs) nil))
    (register-component 'bad :persist t :install (lambda (c x) (declare (ignore c x)) (error "expected")))
    (unwind-protect
         (progn
           (install-component 'empty)
           (log-event :install-commit 'empty :path path)
           (ok (null (getf (recover :path path) :reinstalled)) "already active empty component is borrowed")
           (toggle-component 'empty)
           (recover :path path)
           (ok (eq (getf (gethash 'empty *component-registry*) :status) :suspended)
               "session suspension is respected")
           (ok (= installs 1))
           (log-event :install-commit 'bad :path path)
           (let ((result (recover :path path)))
             (ok (member "YEDITOR-CONTEXT-DURABILITY-TESTS::BAD" (getf result :skipped) :test #'equal)))
           (ok (equal (last (read-events path) 2) '("install-begin" "install-abort"))
               "failed recovery is audited to the explicit path even with durability disabled")
           (ok (not (member "YEDITOR-CONTEXT-DURABILITY-TESTS::BAD" (replay-log :path path) :test #'equal))
               "failed reinstall does not remain falsely committed"))
      (uninstall-component 'empty)
      (delete-file path))))

(deftest test-recovery-100-reversible-cycles
  (let ((path (tmp-log "cycles"))
        (*component-registry* (make-hash-table :test #'eq))
        (*lifecycle-hook* nil)
        (value :baseline))
    (register-component 'cycling :persist t
                        :install (lambda (c x)
                                   (declare (ignore x))
                                   (let ((before value))
                                     (list (yeditor-tx:install-effect
                                            c :variable (lambda () (setf value :active))
                                            :undo-thunk (lambda () (setf value before)))))))
    (unwind-protect
         (dotimes (i 100)
           (log-event :install-commit 'cycling :path path)
           (ok (= 1 (length (getf (recover :path path) :reinstalled))))
           (ok (eq value :active))
           (uninstall-component 'cycling)
           (ok (eq value :baseline) "freshly reconstructed inverse restores exact session baseline"))
      (uninstall-component 'cycling)
      (delete-file path))))

(deftest test-recovery-never-applies-effects-without-inverses
  (let ((path (tmp-log "inverse"))
        (*component-registry* (make-hash-table :test #'eq))
        (*lifecycle-hook* nil)
        (value :baseline))
    (register-component 'irreversible :persist t
                        :install (lambda (c x)
                                   (declare (ignore x))
                                   (list (yeditor-tx:install-effect
                                          c :variable (lambda () (setf value :leaked))))))
    (unwind-protect
         (progn
           (log-event :install-commit 'irreversible :path path)
           (let ((result (recover :path path)))
             (ok (null (getf result :reinstalled)))
             (ok (= 1 (length (getf result :errors))) "rejection is reported with its condition"))
           (ok (eq value :baseline) "missing inverse is rejected before any effect is executed")
           (ok (equal (read-events path) '("install-commit" "install-begin" "install-abort"))))
      (delete-file path))))

(deftest test-recovery-preserves-layer-order-and-hook-path
  (let* ((path (tmp-log "layers"))
         (other (tmp-log "other-journal"))
         (*component-registry* (make-hash-table :test #'eq))
         (*lifecycle-hook* nil)
         (*effects-log-path* other)
         (value :baseline))
    (dolist (component '(layer-a layer-b))
      (let ((layer component))
        (register-component layer :persist t
                            :install (lambda (c x)
                                       (declare (ignore x))
                                       (let ((before value))
                                         (list (yeditor-tx:install-effect
                                                c :variable (lambda () (setf value layer))
                                                :undo-thunk (lambda () (setf value before)))))))))
    (unwind-protect
         (progn
           (dolist (component '(layer-a layer-b))
             (log-event :install-commit component :path path))
           (log-event :suspend 'layer-a :path path)
           (log-event :resume 'layer-a :path path)
           (enable-durability :path other)
           (let ((result (recover :path path)))
             (ok (= 2 (length (getf result :reinstalled))))
             (ok (eq value 'layer-a) "latest override remains on top after recovery"))
           (ok (not (probe-file other)) "recovery does not leak events into the enabled journal")
           (ok (= 8 (length (read-events path))) "each recovery transition is logged exactly once")
           (disable-durability)
           (uninstall-component 'layer-a)
           (ok (eq value 'layer-b))
           (uninstall-component 'layer-b)
           (ok (eq value :baseline)))
      (disable-durability)
      (uninstall-component 'layer-a)
      (uninstall-component 'layer-b)
      (dolist (file (list path other)) (when (probe-file file) (delete-file file))))))

(deftest test-torn-multibyte-write
  (let ((path (tmp-log "utf8")))
    (unwind-protect
         (progn
           (log-event :install-begin "P::PENDING" :path path)
           (with-open-file (out path :direction :output :if-exists :append
                                    :element-type '(unsigned-byte 8))
             (write-byte #xc3 out))
           (let ((result (recover :path path)))
             (ok (equal (getf result :cancelled) '("P::PENDING")))
             (ok (= 1 (getf result :ignored-records))))
           (multiple-value-bind (live pending ignored) (replay-log :path path)
             (ok (and (null live) (null pending) (= ignored 1))
                 "a partial UTF-8 codepoint cannot prevent or erase cancellation")))
      (delete-file path))))
