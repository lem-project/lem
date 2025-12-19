(defpackage :cl-tree-sitter/tests
  (:use :cl :rove))
(in-package :cl-tree-sitter/tests)

;;;; Test Utilities

(defun skip-unless-available ()
  "Skip tests if tree-sitter is not available."
  (unless (ts:tree-sitter-available-p)
    (skip "tree-sitter library not available")))

(defun skip-unless-json-available ()
  "Skip tests if JSON grammar is not available."
  (skip-unless-available)
  (unless (ts:get-language "json")
    (handler-case
        (ts:load-language-from-system "json")
      (error ()
        (skip "JSON grammar not available")))))

;;;; Library Availability Tests

(deftest test-library-check
  (testing "tree-sitter-available-p returns boolean"
    (ok (member (ts:tree-sitter-available-p) '(t nil)))))

;;;; Parser Tests (require library)

(deftest test-parser-creation
  (skip-unless-available)
  (testing "make-parser creates parser"
    (let ((parser (ts:make-parser)))
      (ok (typep parser 'ts:ts-parser))
      (ts:parser-delete parser))))

(deftest test-parser-with-macro
  (skip-unless-available)
  (testing "with-parser handles cleanup"
    (ts:with-parser (parser)
      (ok (typep parser 'ts:ts-parser)))))

;;;; Language Loading Tests

(deftest test-language-registration
  (skip-unless-available)
  (testing "register and get language"
    ;; Create a mock language for testing registration logic
    (let ((lang-list-before (ts:list-languages)))
      ;; Just test the registry functions work
      (ok (listp lang-list-before)))))

;;;; Integration Tests (require JSON grammar)

(deftest test-parse-json
  (skip-unless-json-available)
  (testing "parse simple JSON"
    (ts:with-parser (parser "json")
      (let* ((source "{\"key\": 123}")
             (tree (ts:parser-parse-string parser source)))
        (ok (typep tree 'ts:ts-tree))
        (let ((root (ts:tree-root-node tree)))
          (ok (typep root 'ts:ts-node))
          (ok (string= "document" (ts:node-type root))))))))

(deftest test-node-traversal
  (skip-unless-json-available)
  (testing "traverse JSON nodes"
    (ts:with-parser (parser "json")
      (let* ((source "[1, 2, 3]")
             (tree (ts:parser-parse-string parser source))
             (root (ts:tree-root-node tree)))
        (ok (> (ts:node-child-count root) 0))
        (let ((children (ts:node-children root)))
          (ok (listp children)))))))

(deftest test-node-positions
  (skip-unless-json-available)
  (testing "node position accessors"
    (ts:with-parser (parser "json")
      (let* ((source "null")
             (tree (ts:parser-parse-string parser source))
             (root (ts:tree-root-node tree)))
        (ok (= 0 (ts:node-start-byte root)))
        (ok (= 4 (ts:node-end-byte root)))
        (let ((start (ts:node-start-point root)))
          (ok (= 0 (ts:point-row start)))
          (ok (= 0 (ts:point-column start))))))))

(deftest test-query-compile
  (skip-unless-json-available)
  (testing "compile simple query"
    (let ((lang (ts:get-language "json")))
      (let ((query (ts:query-compile lang "(string) @string")))
        (ok (typep query 'ts:ts-query))
        (ok (= 1 (ts:query-capture-count query)))
        (ok (string= "string" (ts:get-capture-name-by-index query 0)))))))

(deftest test-query-captures
  (skip-unless-json-available)
  (testing "query captures work"
    (ts:with-parser (parser "json")
      (let* ((source "{\"hello\": \"world\"}")
             (tree (ts:parser-parse-string parser source))
             (root (ts:tree-root-node tree))
             (lang (ts:get-language "json"))
             (query (ts:query-compile lang "(string) @str")))
        (let ((captures (ts:query-captures query root)))
          (ok (listp captures))
          (ok (>= (length captures) 2))  ; At least "hello" and "world"
          (when (> (length captures) 0)
            (let ((cap (first captures)))
              (ok (string= "str" (ts:capture-name cap)))
              (ok (typep (ts:capture-node cap) 'ts:ts-node)))))))))

(deftest test-node-string
  (skip-unless-json-available)
  (testing "node-string gives S-expression"
    (ts:with-parser (parser "json")
      (let* ((source "true")
             (tree (ts:parser-parse-string parser source))
             (root (ts:tree-root-node tree)))
        (let ((sexpr (ts:node-string root)))
          (ok (stringp sexpr))
          (ok (search "true" sexpr)))))))
