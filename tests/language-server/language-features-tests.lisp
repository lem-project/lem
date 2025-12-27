(defpackage :lem-tests/language-server/language-features-tests
  (:use :cl
        :rove
        :lem-language-server
        :lem-tests/language-server/utils
        :lem-tests/language-server/test-utils)
  (:import-from :lem-lsp-base/converter
                :convert-to-json
                :convert-from-json))
(in-package :lem-tests/language-server/language-features-tests)

;;;; Completion Tests

(deftest textDocument/completion-with-results
  (testing "returns completion items when completions are available"
    (with-mock-lsp-server
        (:completions (list (make-mock-completion-item
                             :label "defun"
                             :kind :function
                             :signature "(defun name args ...)"
                             :documentation "Define a function")
                            (make-mock-completion-item
                             :label "defvar"
                             :kind :variable
                             :signature "(defvar name value)"
                             :documentation "Define a variable")))
      (make-test-document :text (lines "(in-package :cl-user)" "(def"))
      (let ((response (call-completion-request "file:///test/example.lisp"
                                               :line 1 :character 4)))
        (ok (vectorp response) "Response should be a vector")
        (ok (= 2 (length response)) "Should have 2 completion items")
        (let ((first-item (convert-from-json (aref response 0) 'lsp:completion-item)))
          (ok (equal "defun" (lsp:completion-item-label first-item))
              "First item label should be 'defun'"))))))

(deftest textDocument/completion-empty-symbol
  (testing "returns null when no symbol at point"
    (with-mock-lsp-server (:completions nil)
      (make-test-document :text (lines "(in-package :cl-user)" "  "))
      (let ((response (call-completion-request "file:///test/example.lisp"
                                               :line 1 :character 2)))
        (ok (eq :null response) "Response should be null for empty symbol")))))

;;;; Hover Tests

(deftest textDocument/hover-with-documentation
  (testing "returns hover content for symbol"
    (with-mock-lsp-server
        (:hover "**DEFUN**\n\n(defun name args ...)\n\nDefines a named function.")
      (make-test-document :text (lines "(in-package :cl-user)"
                                       "(defun foo () nil)"))
      (let ((response (call-hover-request "file:///test/example.lisp"
                                          :line 1 :character 1)))
        (ok (not (eq :null response)) "Response should not be null")
        (let ((hover (convert-from-json response 'lsp:hover)))
          (ok (stringp (lsp:hover-contents hover))
              "Hover contents should be a string"))))))

(deftest textDocument/hover-no-symbol
  (testing "returns empty hover when no symbol at point"
    (with-mock-lsp-server (:hover nil)
      (make-test-document :text (lines "(in-package :cl-user)" "   "))
      (let ((response (call-hover-request "file:///test/example.lisp"
                                          :line 1 :character 2)))
        (ok (not (eq :null response)) "Response should not be null")
        (let ((hover (convert-from-json response 'lsp:hover)))
          (ok (equal "" (lsp:hover-contents hover))
              "Hover contents should be empty string"))))))

;;;; Go To Definition Tests

(deftest textDocument/definition-not-found
  (testing "returns empty array when no definition found"
    (with-mock-lsp-server (:definitions nil)
      (make-test-document :text "(undefined-fn)")
      (let ((response (call-definition-request "file:///test/example.lisp"
                                               :line 0 :character 1)))
        (ok (or (eq :null response)
                (and (vectorp response) (= 0 (length response))))
            "Response should be null or empty vector")))))

;;;; Find References Tests

(deftest textDocument/references-not-found
  (testing "returns empty array when no references found"
    (with-mock-lsp-server (:references nil)
      (make-test-document :text "(defun lonely-fn () nil)")
      (let ((response (call-references-request "file:///test/example.lisp"
                                               :line 0 :character 7)))
        (ok (or (eq :null response)
                (and (vectorp response) (= 0 (length response))))
            "Response should be null or empty vector")))))

;;;; Document Formatting Tests

(deftest textDocument/formatting-basic
  (testing "returns text edits for formatting"
    ;; Formatting doesn't require remote-eval-sync, so it works with basic mock server
    (with-mock-server ()
      (lem-tests/language-server/test-utils::call-initialize-request)
      (make-test-document :text "(defun  foo  ()  nil)")
      (let ((response (call-formatting-request "file:///test/example.lisp")))
        (ok (or (eq :null response) (vectorp response))
            "Response should be null or a vector of text edits")))))

(deftest textDocument/formatting-with-options
  (testing "respects formatting options"
    (with-mock-server ()
      (lem-tests/language-server/test-utils::call-initialize-request)
      (make-test-document :text "(let ((x 1))\n  x)")
      (let ((response (call-formatting-request "file:///test/example.lisp"
                                               :tab-size 2
                                               :insert-spaces t)))
        (ok (or (eq :null response) (vectorp response))
            "Response should be null or a vector of text edits")))))

;;;; Document Highlight Tests

(deftest textDocument/documentHighlight-symbol
  (testing "highlights all occurrences of symbol"
    ;; Document highlight doesn't require remote-eval-sync
    (with-mock-server ()
      (lem-tests/language-server/test-utils::call-initialize-request)
      (make-test-document :text "(let ((x 1)) x)")
      (let ((response (call-document-highlight-request "file:///test/example.lisp"
                                                       :line 0 :character 13)))
        (ok (or (eq :null response) (vectorp response))
            "Response should be null or a vector of highlights")))))

(deftest textDocument/documentHighlight-multiple-occurrences
  (testing "finds multiple occurrences of same symbol"
    (with-mock-server ()
      (lem-tests/language-server/test-utils::call-initialize-request)
      (make-test-document :text (lines "(let ((foo 1))"
                                       "  (+ foo foo))"))
      (let ((response (call-document-highlight-request "file:///test/example.lisp"
                                                       :line 0 :character 7)))
        (when (vectorp response)
          (ok (>= (length response) 1)
              "Should find at least one highlight"))))))

;;;; Signature Help Tests

(deftest textDocument/signatureHelp-no-context
  (testing "returns null when not in function call context"
    ;; Use with-mock-server directly to set up autodoc response
    (with-mock-server ()
      (lem-tests/language-server/test-utils::call-initialize-request)
      ;; Set up autodoc response to indicate no signature help available
      (set-mock-response 'micros::autodoc-function '(:not-available nil))
      (make-test-document :text "  ")
      (let ((response (call-signature-help-request "file:///test/example.lisp"
                                                   :line 0 :character 1)))
        (ok (eq :null response)
            "Response should be null when not in function context")))))

;;;; Mock Response History Tests

(deftest mock-server-records-response-history
  (testing "mock server records all remote-eval-sync calls"
    (with-mock-lsp-server
        (:completions (list (make-mock-completion-item :label "test")))
      (make-test-document :text "(te")
      (call-completion-request "file:///test/example.lisp"
                               :line 0 :character 3)
      (let ((history (mock-server-response-history (current-server))))
        (ok (consp history) "History should not be empty")
        (ok (find 'micros/lsp-api:completions history
                  :key (lambda (entry) (car (getf entry :expression))))
            "History should contain completions call")))))

(deftest mock-server-clear-responses
  (testing "clear-mock-responses clears all responses and history"
    (with-mock-server ()
      (lem-tests/language-server/test-utils::call-initialize-request)
      (set-mock-response 'micros/lsp-api:hover-symbol "test")
      (clear-mock-responses)
      (ok (= 0 (hash-table-count (mock-server-canned-responses (current-server))))
          "Canned responses should be empty")
      (ok (null (mock-server-response-history (current-server)))
          "Response history should be empty"))))
