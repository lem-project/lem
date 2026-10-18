(in-package :cl-user)

;; NOTE (controller): no runtime (require ...) — the test system chain (registry-tests
;; -> yeditor-context -> yeditor-tx -> rove) provides all packages; entry point is
;; scripts/run-yeditor-registry-tests.lisp.

;; Dummy component definition
(yeditor-context:define-component dummy-component
  :version "1.0"
  :author "test"
  :depends-on nil
  :install (lambda (comp ctx)
             ;; create 10 effects, each installing a dummy var and returning handle
             (let ((handles nil))
               (dotimes (i 10)
                 ;; capture the CURRENT value of i (SBCL dotimes shares the binding)
                 (let ((dummy-sym (intern (format nil "*DUMMY~A*" i))))
                   (let ((h (yeditor-tx:install-effect comp :dummy
                            (lambda () (setf (symbol-value dummy-sym) t))
                            :undo-thunk (lambda () (setf (symbol-value dummy-sym) nil)))))
                     (push h handles))))
               (reverse handles)))
  :activate nil
  :deactivate nil
  :config-spec nil)

(defun clear-dummy-vars ()
  (dotimes (i 10)
    (setf (symbol-value (intern (format nil "*DUMMY~A*" i))) nil)))

(rove:deftest test-install-and-toggle
  (clear-dummy-vars)
  ;; install
  (let ((comp-entry (yeditor-context:install-component 'dummy-component)))
    (rove:ok (equal (length (getf comp-entry :state)) 10) "install yields 10 handles")
    ;; all dummy vars true
    (dotimes (i 10)
      (rove:ok (symbol-value (intern (format nil "*DUMMY~A*" i)))
               (format nil "dummy var *DUMMY~A* set" i)))
    ;; suspend
    (yeditor-context:toggle-component 'dummy-component)
    ;; after suspend, all dummy vars nil
    (dotimes (i 10)
      (rove:ok (not (symbol-value (intern (format nil "*DUMMY~A*" i))))
               (format nil "dummy var *DUMMY~A* cleared after suspend" i)))
    ;; resume
    (yeditor-context:toggle-component 'dummy-component)
    (dotimes (i 10)
      (rove:ok (symbol-value (intern (format nil "*DUMMY~A*" i)))
               (format nil "dummy var *DUMMY~A* restored after resume" i))))
  ;; 1000 toggle loop
  (dotimes (n 1000)
    (yeditor-context:toggle-component 'dummy-component)
    (dotimes (i 10)
      ;; previous round ends RESUMED; toggle #0 suspends (nil), toggle #1 resumes (t), ...
      (if (evenp n)
          (rove:ok (not (symbol-value (intern (format nil "*DUMMY~A*" i)))))
          (rove:ok (symbol-value (intern (format nil "*DUMMY~A*" i)))))))
  (clear-dummy-vars))

;; Test install error rollback
(yeditor-context:define-component faulty-component
  :version "1.0"
  :author "test"
  :depends-on nil
  :install (lambda (comp ctx)
             (yeditor-tx:install-effect comp :bad (lambda () (error "boom")) :undo-thunk (lambda ()))
             nil)
  :activate nil
  :deactivate nil
  :config-spec nil)

(rove:deftest test-install-error
  (handler-case
      (yeditor-context:install-component 'faulty-component)
    (error (e)
      (rove:ok (string= (string (type-of e)) "SIMPLE-ERROR") "error signaled"))
    (:no-error ()
      (rove:fail "install should have signaled error"))))

;; Test dependency missing
(yeditor-context:define-component dependent-component
  :version "1.0"
  :author "test"
  :depends-on '(nonexistent)
  :install (lambda (c ctx) nil)
  :activate nil
  :deactivate nil
  :config-spec nil)

(rove:deftest test-missing-dep
  (handler-case
      (yeditor-context:install-component 'dependent-component)
    (error (e)
      (rove:ok (search "nonexistent" (princ-to-string e) :test #'string-equal) "missing dependency reported"))
    (:no-error ()
      (rove:fail "install should have failed due to missing dep"))))

;; Test idempotent uninstall
(rove:deftest test-uninstall-idempotent
  (yeditor-context:install-component 'dummy-component)
  (yeditor-context:uninstall-component 'dummy-component)
  (rove:ok (null (getf (gethash 'dummy-component yeditor-context::*component-registry*) :state)) "state nil after uninstall")
  (yeditor-context:uninstall-component 'dummy-component) ; second call should be safe
  (rove:ok t "second uninstall no error"))
