(defpackage :lem-tests/lsp-mode/tests
  (:use :cl
        :rove
        :lem
        :lem-lsp-mode)
  (:import-from :lem-tests/lsp-mode/mock-client
                :mock-client
                :make-mock-client
                :set-mock-client-response
                :mock-client-request-history
                :mock-client-should-error-p)
  (:import-from :lem-tests/lsp-mode/test-utils
                :with-mock-lsp-workspace
                :make-test-server-capabilities
                :make-mock-completion-response
                :make-mock-hover-response
                :make-test-completion-params
                :make-test-hover-params))
(in-package :lem-tests/lsp-mode/tests)

;;;; Capability Detection Tests

(deftest test-provide-completion-p
  (testing "returns true when completion capability is present"
    (let ((ws (make-instance 'lem-lsp-mode::workspace
                             :server-capabilities (make-test-server-capabilities
                                                   :completion-provider t))))
      (ok (lem-lsp-mode::provide-completion-p ws)
          "Should detect completion capability")))

  (testing "returns nil when completion capability is absent"
    (let ((ws (make-instance 'lem-lsp-mode::workspace
                             :server-capabilities (make-test-server-capabilities
                                                   :completion-provider nil))))
      (ok (not (lem-lsp-mode::provide-completion-p ws))
          "Should not detect completion capability when absent"))))

(deftest test-provide-hover-p
  (testing "returns true when hover capability is present"
    (let ((ws (make-instance 'lem-lsp-mode::workspace
                             :server-capabilities (make-test-server-capabilities
                                                   :hover-provider t))))
      (ok (lem-lsp-mode::provide-hover-p ws)
          "Should detect hover capability")))

  (testing "returns nil when hover capability is absent"
    (let ((ws (make-instance 'lem-lsp-mode::workspace
                             :server-capabilities (make-test-server-capabilities
                                                   :hover-provider nil))))
      (ok (not (lem-lsp-mode::provide-hover-p ws))
          "Should not detect hover capability when absent"))))

(deftest test-provide-definition-p
  (testing "returns true when definition capability is present"
    (let ((ws (make-instance 'lem-lsp-mode::workspace
                             :server-capabilities (make-test-server-capabilities
                                                   :definition-provider t))))
      (ok (lem-lsp-mode::provide-definition-p ws)
          "Should detect definition capability"))))

;;;; Mock Client Tests

(deftest test-mock-client-returns-canned-response
  (testing "mock client returns configured response"
    (let ((client (make-mock-client)))
      (set-mock-client-response client "textDocument/completion"
                                (make-mock-completion-response
                                 '((:label "test" :kind 1))))
      (let ((callback-result nil))
        (lem-language-client/request:request-async
         client
         (make-instance 'lsp:text-document/completion)
         (make-test-completion-params)
         (lambda (response)
           (setf callback-result response)))
        (ok callback-result "Callback should be called")
        (ok (or (vectorp callback-result)
                (listp callback-result))
            "Response should be completion items")))))

(deftest test-mock-client-records-request-history
  (testing "mock client records all requests made"
    (let ((client (make-mock-client)))
      (lem-language-client/request:request-async
       client
       (make-instance 'lsp:text-document/completion)
       (make-test-completion-params)
       (lambda (response) (declare (ignore response))))
      (let ((history (mock-client-request-history client)))
        (ok (= 1 (length history)) "Should have one request in history")
        (ok (equal "textDocument/completion"
                   (getf (first history) :method))
            "Request method should be recorded")))))

(deftest test-mock-client-error-callback
  (testing "mock client calls error callback when should-error-p is true"
    (let ((client (make-mock-client :should-error-p t))
          (error-called nil))
      (lem-language-client/request:request-async
       client
       (make-instance 'lsp:text-document/hover)
       (make-test-hover-params)
       (lambda (response) (declare (ignore response)))
       (lambda (message code)
         (declare (ignore message code))
         (setf error-called t)))
      (ok error-called "Error callback should be called"))))

;;;; Workspace Management Tests

(deftest test-workspace-class-structure
  (testing "workspace has expected slots"
    (let ((ws (make-instance 'lem-lsp-mode::workspace
                             :client (make-mock-client)
                             :root-uri "file:///test"
                             :server-capabilities (make-test-server-capabilities))))
      (ok (lem-lsp-mode::workspace-client ws) "Should have client")
      (ok (lem-lsp-mode::workspace-root-uri ws) "Should have root-uri")
      (ok (lem-lsp-mode::workspace-server-capabilities ws)
          "Should have server-capabilities"))))

;;;; Server Capabilities Tests

(deftest test-server-capabilities-structure
  (testing "server capabilities are properly structured"
    (let ((caps (make-test-server-capabilities
                 :hover-provider t
                 :completion-provider t
                 :definition-provider t)))
      (ok (lsp:server-capabilities-hover-provider caps)
          "Should have hover provider")
      (ok (lsp:server-capabilities-completion-provider caps)
          "Should have completion provider")
      (ok (lsp:server-capabilities-definition-provider caps)
          "Should have definition provider"))))

;;;; URI Conversion Tests

(deftest test-pathname-to-uri
  (testing "converts pathname to file URI"
    (let ((uri (lem-lsp-mode::pathname-to-uri "/home/user/test.lisp")))
      (ok (stringp uri) "Result should be a string")
      (ok (search "file://" uri) "Should start with file://"))))
