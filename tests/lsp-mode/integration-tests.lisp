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

(defun skip-if-not-installed (server-command server-name)
  "Skip the test if the LSP server is not installed."
  (unless *run-integration-tests-p*
    (skip (format nil "Integration tests disabled (set LEM_RUN_INTEGRATION_TESTS=1 to enable)")))
  (unless (lsp-server-installed-p server-command)
    (skip (format nil "~A not installed (command: ~A)" server-name server-command))))

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
     (skip-if-not-installed ,server-command ,server-name)
     ,@body))

;;; Note: Full integration tests require a running event loop and proper
;;; LSP server lifecycle management. The tests below are placeholders
;;; that demonstrate the intended test structure.

;;;; gopls Integration Tests

(defintegration-test gopls-server-installed
    (:server-command "gopls" :server-name "gopls (Go LSP)")
  (testing "gopls is available"
    (ok (lsp-server-installed-p "gopls")
        "gopls should be installed")))

;;;; typescript-language-server Integration Tests

(defintegration-test typescript-server-installed
    (:server-command "typescript-language-server" :server-name "TypeScript LSP")
  (testing "typescript-language-server is available"
    (ok (lsp-server-installed-p "typescript-language-server")
        "typescript-language-server should be installed")))

;;; Test runner

(defun run-integration-tests ()
  "Run all integration tests.
Set LEM_RUN_INTEGRATION_TESTS=1 environment variable to enable."
  (let ((*run-integration-tests-p* t))
    (rove:run :lem-tests/lsp-mode/integration-tests)))
