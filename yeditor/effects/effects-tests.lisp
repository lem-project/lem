(defpackage :yeditor-effects-tests
  (:use :cl :yeditor-context :yeditor-effects :yeditor-tx :rove))
(in-package :yeditor-effects-tests)

;; dummy hook function
(defun dummy-hook-fn (&rest _args) (declare (ignore _args)))

(yeditor-context:define-component test-effects
  :install (lambda (comp ctx)
             (list (install-buffer comp "test-buffer")
                   (install-mode comp (lem:current-buffer) (lambda (buf) (declare (ignore buf))))
                   (install-hook comp (lem:variable-value 'after-save-hook :global) #'dummy-hook-fn))))

(deftest install-and-uninstall-buffer
  (let ((comp 'test-effects))
    (yeditor-context:install-component comp)
    ;; ensure buffer exists
    (assert (find "test-buffer" (lem:buffer-list) :key (lambda (b) (lem:buffer-name b)) :test #'string=))
    (yeditor-context:uninstall-component comp)
    (assert (null (lem:buffer-list)))
    (assert (null (lem:variable-value 'after-save-hook :global)))))
