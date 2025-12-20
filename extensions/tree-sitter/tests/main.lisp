(defpackage :lem-tree-sitter/tests
  (:use :cl :rove)
  (:local-nicknames (:ts :tree-sitter)
                    (:lem-ts :lem-tree-sitter))
  (:import-from :lem-yaml-mode))
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

;;;; YAML Buffer Attribute Tests

(defun line-has-attribute-p (line)
  "Check if a line has any syntax highlighting attributes."
  (getf (lem/buffer/line:line-plist line) :attribute))

(defun get-line-at (buffer line-number)
  "Get the line object at LINE-NUMBER (1-indexed) in BUFFER."
  (lem:with-point ((p (lem:buffer-start-point buffer)))
    (lem:line-offset p (1- line-number))
    (lem/buffer/internal::point-line p)))

(defun skip-unless-yaml-available ()
  "Skip tests if tree-sitter or YAML grammar is not available."
  (unless (lem-ts:tree-sitter-available-p)
    (skip "tree-sitter library not available"))
  (handler-case
      (ts:load-language-from-system "yaml")
    (error ()
      (skip "YAML grammar not available"))))

(defun get-parser-highlight-query (parser)
  "Get the highlight query from the parser."
  (slot-value parser 'lem-tree-sitter::highlight-query))

(deftest test-yaml-buffer-attributes
  (skip-unless-yaml-available)
  (testing "tree-sitter applies attributes to YAML buffer"
    (lem:with-current-buffers ()
      (let* ((yaml-content "# Comment line
name: value
enabled: true
count: 42
")
             ;; Create a fresh syntax table for testing
             (syntax-table (lem:make-syntax-table))
             (buffer (lem:make-buffer "*yaml-test*"
                                      :temporary t
                                      :enable-undo-p nil
                                      :syntax-table syntax-table)))
        ;; Enable syntax highlighting for this buffer
        (setf (lem:variable-value 'lem:enable-syntax-highlight :buffer buffer) t)
        ;; Insert YAML content
        (lem:insert-string (lem:buffer-point buffer) yaml-content)
        (lem:buffer-start (lem:buffer-point buffer))
        ;; Create tree-sitter parser with highlight query
        (let* ((query-path (asdf:system-relative-pathname
                            :lem-tree-sitter "queries/yaml/highlights.scm"))
               (parser (lem-ts:make-treesitter-parser
                        "yaml" :highlight-query-path query-path)))
          (testing "parser is created"
            (ok parser))
          (testing "query file exists"
            (ok (probe-file query-path)))
          (testing "highlight query is loaded"
            (ok (get-parser-highlight-query parser)))
          ;; Set the parser on the syntax table
          (lem:set-syntax-parser syntax-table parser)
          ;; Trigger syntax scanning
          (lem:syntax-scan-region (lem:buffer-start-point buffer)
                                  (lem:buffer-end-point buffer))
          ;; Verify attributes are applied
          (testing "comment line has attributes"
            (ok (line-has-attribute-p (get-line-at buffer 1))))
          (testing "key-value line has attributes"
            (ok (line-has-attribute-p (get-line-at buffer 2))))
          (testing "boolean value line has attributes"
            (ok (line-has-attribute-p (get-line-at buffer 3))))
          (testing "number value line has attributes"
            (ok (line-has-attribute-p (get-line-at buffer 4)))))))))
