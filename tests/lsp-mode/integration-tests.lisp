(defpackage :lem-tests/lsp-mode/integration-tests
  (:use :cl
        :rove
        :lem
        :lem-lsp-mode)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:export :run-integration-tests
           :*integration-test-timeout*))
(in-package :lem-tests/lsp-mode/integration-tests)

;;; Configuration

(defvar *integration-test-timeout* 30
  "Timeout in seconds for integration tests.")

(defvar *run-integration-tests-p*
  (uiop:getenv "LEM_RUN_INTEGRATION_TESTS")
  "If non-nil, run integration tests that require external LSP servers.")

;;; Utility functions

(defun lsp-server-installed-p (command)
  "Check if an LSP server command is available in PATH."
  (ignore-errors
    (zerop (nth-value 2 (uiop:run-program (list "which" command)
                                           :ignore-error-status t)))))

(defun should-run-integration-test-p (server-command server-name)
  "Check if integration test should run. Returns T if it should run, NIL if skipped."
  (cond
    ((not *run-integration-tests-p*)
     (skip (format nil "Integration tests disabled (set LEM_RUN_INTEGRATION_TESTS=1 to enable)"))
     nil)
    ((not (lsp-server-installed-p server-command))
     (skip (format nil "~A not installed (command: ~A)" server-name server-command))
     nil)
    (t t)))

(defmacro with-temp-directory ((dir-var) &body body)
  "Execute BODY with DIR-VAR bound to a temporary directory path."
  `(let ((,dir-var (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "lem-test-~A/" (random 100000))
                                     (uiop:temporary-directory)))))
     (unwind-protect
          (progn
            (ensure-directories-exist ,dir-var)
            ,@body)
       (uiop:delete-directory-tree ,dir-var :validate t :if-does-not-exist :ignore))))

(defmacro with-test-file ((filename content &key (dir-var (gensym))) &body body)
  "Create a temporary file with CONTENT and execute BODY."
  `(with-temp-directory (,dir-var)
     (let ((filepath (merge-pathnames ,filename ,dir-var)))
       (with-open-file (out filepath :direction :output :if-exists :supersede)
         (write-string ,content out))
       ,@body)))

;;; Integration test macros

(defmacro defintegration-test (name (&key server-command server-name) &body body)
  "Define an integration test that requires an external LSP server."
  `(deftest ,name
     (when (should-run-integration-test-p ,server-command ,server-name)
       ,@body)))

;;; Note: Full integration tests require a running event loop and proper
;;; LSP server lifecycle management. The macros above (defintegration-test,
;;; with-test-file, etc.) provide the infrastructure for writing such tests.
;;;
;;; To add integration tests:
;;; 1. Set LEM_RUN_INTEGRATION_TESTS=1 environment variable
;;; 2. Ensure the required LSP server is installed
;;; 3. Use defintegration-test macro to define tests that will skip
;;;    gracefully when the server is not available

;;;; gopls Integration Tests

(defintegration-test gopls-server-available
    (:server-command "gopls" :server-name "gopls (Go LSP)")
  (testing "gopls can be executed"
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program '("gopls" "version")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore output error-output))
      (ok (zerop exit-code) "gopls version command should succeed"))))

;;;; typescript-language-server Integration Tests

(defintegration-test typescript-server-available
    (:server-command "typescript-language-server" :server-name "TypeScript LSP")
  (testing "typescript-language-server can be executed"
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program '("typescript-language-server" "--version")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore output error-output))
      (ok (zerop exit-code) "typescript-language-server --version should succeed"))))

;;; Test runner

(defun run-integration-tests ()
  "Run all integration tests.
Set LEM_RUN_INTEGRATION_TESTS=1 environment variable to enable."
  (let ((*run-integration-tests-p* t))
    (rove:run :lem-tests/lsp-mode/integration-tests)))
