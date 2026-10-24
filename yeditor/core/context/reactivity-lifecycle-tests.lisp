;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context-reactivity-lifecycle-tests
  (:use :cl :rove :yeditor-context :yeditor-context/reactivity :yeditor-tx))
(in-package :yeditor-context-reactivity-lifecycle-tests)

(defmacro with-context (&body body)
  `(let ((*component-registry* (make-hash-table :test #'eq))
         (yeditor-context/reactivity::*coeffect-registry* (make-hash-table :test #'eq))
         (yeditor-context/reactivity::*current-context* (list :window-width 0))
         (yeditor-context/reactivity::*reclassifying* nil)
         (yeditor-context/reactivity::*reclassify-pending* nil)
         (*agent-tx-active* nil) (*eval-overlay-active* nil) (*diagnostics-critical* nil)
         (*tx-pending* nil) (*tx-committed* nil))
     ,@body))

(defun active-p (name) (yeditor-context/reactivity::component-active-p name))

(deftest declarative-requires-and-empty-components
  (with-context
    (let ((installs 0))
      (apply #'register-component
             (list 'empty :requires '(:buffer-mode python-mode)
                   :install (lambda (comp ctx) (declare (ignore comp ctx)) (incf installs) nil)))
      (update-context :buffer-mode 'python-mode)
      (update-context :buffer-mode 'python-mode)
      (ok (= installs 1) "zero-effect components activate only once")
      (ok (active-p 'empty))
      (update-context :buffer-mode 'java-mode)
      (ok (not (active-p 'empty))))
    (eval '(define-component macro-component :requires '(:remote-p t)
             :install (lambda (comp ctx) (declare (ignore comp ctx)) nil)))
    (update-context :remote-p t)
    (ok (active-p 'macro-component) "DEFINE-COMPONENT accepts :requires")))

(deftest predicates-and-server-statuses
  (with-context
    (dolist (pair '((mode :buffer-mode python-mode) (project :project-type python)
                    (file :file-type "py") (remote :remote-p t)
                    (wide :window-width 80) (server-a :server-online :a)
                    (server-b :server-online :b)))
      (register-component (first pair) :install (lambda (comp ctx)
        (declare (ignore ctx)) (list (install-effect comp :noop (lambda ()) :undo-thunk (lambda ())))))
      (define-coeffects (first pair) (rest pair)))
    (update-contexts :buffer-mode 'python-mode :project-type 'python
                     :file-type "py" :remote-p t :window-width 100)
    (dolist (name '(mode project file remote wide)) (ok (active-p name)))
    (emit-context-event :window-resize 40)
    (ok (not (active-p 'wide)))
    (emit-context-event :window-resize 100)
    (ok (active-p 'wide))
    (emit-context-event :project-change 'java)
    (ok (not (active-p 'project)))
    (emit-context-event :project-change 'python)
    (ok (active-p 'project))
    (emit-context-event :server-status :a :online)
    (emit-context-event :server-status :b :online)
    (ok (active-p 'server-a) "multiple servers stay online independently")
    (ok (active-p 'server-b))
    (emit-context-event :server-status :a :offline)
    (ok (not (active-p 'server-a)))
    (ok (active-p 'server-b) "taking A offline preserves B")
    (update-contexts :buffer-mode 'java-mode :project-type 'java
                     :file-type "java" :remote-p nil :window-width 40)
    (dolist (name '(mode project file remote wide)) (ok (not (active-p name))))))

(deftest buffer-close-and-background-mode-events
  (with-context
    (lem-fake-interface:with-fake-interface ()
      (let ((front (lem:make-buffer "front.py")) (back (lem:make-buffer "back.java")))
        (unwind-protect
             (progn
               (register-component 'mode :install (lambda (comp ctx)
                 (declare (ignore ctx)) (list (install-effect comp :noop (lambda ()) :undo-thunk (lambda ())))))
               (define-coeffects 'mode '(:buffer-mode python-mode))
               (emit-context-event :buffer-open front)
               (emit-context-event :buffer-mode-change front 'python-mode)
               (ok (active-p 'mode))
               (emit-context-event :buffer-mode-change back 'java-mode)
               (ok (active-p 'mode) "background changes do not replace the selected buffer context")
               (emit-context-event :buffer-close back)
               (ok (active-p 'mode))
               (emit-context-event :buffer-close front)
               (ok (not (active-p 'mode)))
               (ok (null (context-value :buffer-mode))))
          (lem:delete-buffer front) (lem:delete-buffer back))))))

(deftest transitions-restore-completed-effects-on-failure
  (with-context
    (let ((first nil) (second nil) (fail nil) (callback-fails nil))
      (register-component 'atomic
       :install (lambda (comp ctx)
                  (declare (ignore ctx))
                  (list (install-effect comp :first (lambda () (setf first t))
                          :undo-thunk (lambda () (when fail (error "inverse fails")) (setf first nil)))
                        (install-effect comp :second (lambda () (setf second t))
                          :undo-thunk (lambda () (setf second nil)))))
       :deactivate (lambda (comp) (declare (ignore comp))
                     (when callback-fails (error "deactivate fails"))))
      (install-component 'atomic)
      (setf fail t)
      (ok (signals (uninstall-component 'atomic) 'error))
      (ok (and first second (active-p 'atomic)) "failed inverse restores completed undos")
      (setf fail nil callback-fails t)
      (ok (signals (uninstall-component 'atomic) 'error))
      (ok (and first second (active-p 'atomic)) "failed deactivate callback preserves active state")
      (setf callback-fails nil)
      (uninstall-component 'atomic)
      (ok (and (null first) (null second) (not (active-p 'atomic)))))))

(deftest activation-callback-is-in-the-subtransaction
  (with-context
    (let ((value nil))
      (register-component 'fails-after-commit
       :install (lambda (comp ctx) (declare (ignore ctx))
                  (list (install-effect comp :value (lambda () (setf value t))
                                        :undo-thunk (lambda () (setf value nil)))))
       :activate (lambda (comp) (declare (ignore comp)) (error "activation fails")))
      (ok (signals (install-component 'fails-after-commit) 'error))
      (ok (null value))
      (ok (not (active-p 'fails-after-commit))))))

(deftest many-components-follow-every-event-and-preserve-outer-tx
  (with-context
    (let ((values (make-array 6 :initial-element nil))
          (outer nil))
      (dotimes (i 6)
        (let ((index i) (name (intern (format nil "PROPERTY-~D" i))))
          (register-component name :install
            (lambda (comp ctx) (declare (ignore ctx))
              (list (install-effect comp :value (lambda () (setf (aref values index) t))
                                    :undo-thunk (lambda () (setf (aref values index) nil))))))
          (define-coeffects name (list :buffer-mode (if (evenp i) 'python-mode 'java-mode)))))
      (install-effect 'outer :outer (lambda () (setf outer t)) :undo-thunk (lambda () (setf outer nil)))
      (dotimes (event 300)
        (let ((python-p (not (zerop (mod event 3)))))
          (emit-context-event :buffer-mode-change nil (if python-p 'python-mode 'java-mode))
          (dotimes (i 6)
            (ok (eql (aref values i) (if (evenp i) python-p (not python-p)))
                "all component effects agree with the latest event"))))
      (ok (not outer))
      (tx-commit) (ok outer) (tx-rollback)
      (emit-context-event :buffer-close nil)
      (ok (every #'null values) "closing restores every component baseline"))))

(deftest config-watcher-reclassifies-on-the-editor-thread
  (with-context
    (let* ((path (merge-pathnames (format nil "t7-config-~A.lisp" (gensym)) (uiop:temporary-directory)))
           (yeditor-context/loader:*applied-components* (make-hash-table :test #'eq))
           (editor-thread (bt2:current-thread))
           (active nil) (activation-thread nil))
      (register-component 'configured
        :install (lambda (comp ctx) (declare (ignore ctx))
                   (list (install-effect comp :active
                           (lambda () (setf active t activation-thread (bt2:current-thread)))
                           :undo-thunk (lambda () (setf active nil))))))
      (define-coeffects 'configured '(:window-width 80))
      (update-context :window-width 40)
      (setf (getf yeditor-context/reactivity::*current-context* :window-width) 100)
      (with-open-file (out path :direction :output) (write-string "" out))
      (unwind-protect
           (progn
             (yeditor-context/loader:start-config-watcher path :interval 0.01)
             (sleep 0.05)
             (ok (not active) "watcher only schedules editor work")
             (loop :repeat 100 :until active :do (lem:receive-event 0.02))
             (ok active "config-change reclassifies declared components")
             (ok (eq editor-thread activation-thread) "activation runs on the editor event thread"))
        (yeditor-context/loader:stop-config-watcher)
        (uninstall-component 'configured)
        (delete-file path)))))

(deftest nonconverging-feedback-is-reported
  (with-context
    (let ((yeditor-context/reactivity:*max-reclassification-passes* 4))
      (register-component 'oscillator
        :install (lambda (comp ctx) (declare (ignore ctx))
                   (list (install-effect comp :feedback
                           (lambda () (update-context :window-width 0))
                           :undo-thunk (lambda () (update-context :window-width 100))))))
      (define-coeffects 'oscillator '(:window-width 80))
      (ok (signals (update-context :window-width 100)
                   'yeditor-context/reactivity:context-convergence-error)
          "feedback never silently leaves the last event unclassified"))))

(deftest failed-reactive-install-is-isolated-from-other-components
  (with-context
    (let ((good nil) (partial nil))
      (register-component 'bad
        :install (lambda (comp ctx) (declare (ignore ctx))
                   (list (install-effect comp :partial (lambda () (setf partial t))
                                         :undo-thunk (lambda () (setf partial nil)))
                         (install-effect comp :fail (lambda () (error "installer fails"))
                                         :undo-thunk (lambda ())))))
      (register-component 'good
        :install (lambda (comp ctx) (declare (ignore ctx))
                   (list (install-effect comp :good (lambda () (setf good t))
                                         :undo-thunk (lambda () (setf good nil))))))
      (define-coeffects 'bad '(:remote-p t))
      (define-coeffects 'good '(:remote-p t))
      (let ((failures (handler-bind ((warning #'muffle-warning)) (update-context :remote-p t))))
        (ok (= 1 (length failures)))
        (ok (eq (getf (first failures) :component) 'bad)))
      (ok (and good (active-p 'good)))
      (ok (and (null partial) (not (active-p 'bad)))))))

(deftest overlapping-components-unwind-and-rebase-in-lifo-order
  (with-context
    (let ((value :baseline))
      (dolist (spec '((outer :window-width 80) (inner :remote-p t)))
        (let ((name (first spec)))
          (register-component name :install
            (lambda (comp ctx) (declare (ignore ctx))
              (let ((previous nil))
                (list (install-effect comp :override
                        (lambda () (setf previous value value comp))
                        :undo-thunk (lambda () (setf value previous)))))))
          (define-coeffects name (rest spec))))
      (update-contexts :window-width 100 :remote-p t)
      (ok (eq value 'inner))
      (let ((handles (getf (gethash 'inner *component-registry*) :state)))
        (emit-context-event :window-resize 40)
        (ok (eq value 'inner) "removing an older layer preserves the newer active override")
        (ok (eq handles (getf (gethash 'inner *component-registry*) :state))))
      (emit-context-event :remote-change nil)
      (ok (eq value :baseline) "final removal restores the baseline, not a stale outer layer"))))
