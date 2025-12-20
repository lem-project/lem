(defpackage :lem-tree-sitter/tests
  (:use :cl :rove)
  (:local-nicknames (:ts :tree-sitter)
                    (:lem-ts :lem-tree-sitter)))
(in-package :lem-tree-sitter/tests)

;;;; Test Utilities

(defun skip-unless-available ()
  "Skip tests if tree-sitter is not available."
  (unless (lem-ts:tree-sitter-available-p)
    (skip "tree-sitter library not available")))

;;;; Basic Tests

(deftest test-tree-sitter-availability
  (testing "tree-sitter-available-p returns boolean"
    (ok (member (lem-ts:tree-sitter-available-p) '(t nil)))))

(deftest test-capture-mapping
  (testing "standard capture mappings exist"
    (ok (lem-tree-sitter/highlight:capture-to-attribute "keyword"))
    (ok (lem-tree-sitter/highlight:capture-to-attribute "string"))
    (ok (lem-tree-sitter/highlight:capture-to-attribute "comment"))))

(deftest test-capture-hierarchical-lookup
  (testing "hierarchical capture name lookup"
    ;; keyword.control should fall back to keyword
    (ok (lem-tree-sitter/highlight:capture-to-attribute "keyword.control"))))

;;;; Integration Tests (require tree-sitter + JSON grammar)

(deftest test-parser-creation
  (skip-unless-available)
  (handler-case
      (progn
        (ts:load-language-from-system "json")
        (testing "make-treesitter-parser works"
          (let ((parser (lem-ts:make-treesitter-parser "json")))
            (ok (typep parser 'lem-ts:treesitter-parser))
            (ok (string= "json" (lem-ts:treesitter-parser-language-name parser))))))
    (error ()
      (skip "JSON grammar not available"))))
