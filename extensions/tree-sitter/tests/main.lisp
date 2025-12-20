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

(defun get-point-at-line (buffer line-number)
  "Get a point at the start of LINE-NUMBER (1-indexed) in BUFFER."
  (lem:with-point ((p (lem:buffer-start-point buffer)))
    (lem:line-offset p (1- line-number))
    (lem:copy-point p :temporary)))

(defun point-has-attribute-p (point)
  "Check if POINT has a syntax highlighting attribute."
  (lem:text-property-at point :attribute))

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

(defun has-attribute-at-position-p (buffer line-number charpos)
  "Check if position (LINE-NUMBER, CHARPOS) has an attribute."
  (lem:with-point ((p (get-point-at-line buffer line-number)))
    (lem:character-offset p charpos)
    (point-has-attribute-p p)))

(deftest test-highlight-position-accuracy
  (skip-unless-yaml-available)
  (testing "tree-sitter applies highlights at correct byte positions"
    (lem:with-current-buffers ()
      ;; Simple test: "key: value" - key should be 0-3, value should be 5-10
      (let* ((yaml-content "key: value")
             (syntax-table (lem:make-syntax-table))
             (buffer (lem:make-buffer "*position-test*"
                                      :temporary t
                                      :enable-undo-p nil
                                      :syntax-table syntax-table)))
        (setf (lem:variable-value 'lem:enable-syntax-highlight :buffer buffer) t)
        (lem:insert-string (lem:buffer-point buffer) yaml-content)
        (lem:buffer-start (lem:buffer-point buffer))
        (let* ((query-path (asdf:system-relative-pathname
                            :lem-tree-sitter "queries/yaml/highlights.scm"))
               (parser (lem-ts:make-treesitter-parser
                        "yaml" :highlight-query-path query-path)))
          (lem:set-syntax-parser syntax-table parser)
          (lem:syntax-scan-region (lem:buffer-start-point buffer)
                                  (lem:buffer-end-point buffer))
          ;; Check attributes at specific positions using public API
          (testing "attributes are applied at start"
            (ok (has-attribute-at-position-p buffer 1 0)))
          ;; "key" is at position 0-2 (chars 0,1,2)
          (testing "key is highlighted at position 0"
            (ok (has-attribute-at-position-p buffer 1 0)))
          (testing "key is highlighted at position 2"
            (ok (has-attribute-at-position-p buffer 1 2)))
          ;; "value" is at position 5-9 (chars 5,6,7,8,9)
          (testing "value is highlighted at position 5"
            (ok (has-attribute-at-position-p buffer 1 5)))
          (testing "value is highlighted at position 9"
            (ok (has-attribute-at-position-p buffer 1 9))))))))

(deftest test-multiline-highlight-positions
  (skip-unless-yaml-available)
  (testing "multi-line highlights have correct positions"
    (lem:with-current-buffers ()
      (let* ((yaml-content "first: 1
second: 2")
             (syntax-table (lem:make-syntax-table))
             (buffer (lem:make-buffer "*multiline-test*"
                                      :temporary t
                                      :enable-undo-p nil
                                      :syntax-table syntax-table)))
        (setf (lem:variable-value 'lem:enable-syntax-highlight :buffer buffer) t)
        (lem:insert-string (lem:buffer-point buffer) yaml-content)
        (lem:buffer-start (lem:buffer-point buffer))
        (let* ((query-path (asdf:system-relative-pathname
                            :lem-tree-sitter "queries/yaml/highlights.scm"))
               (parser (lem-ts:make-treesitter-parser
                        "yaml" :highlight-query-path query-path)))
          (lem:set-syntax-parser syntax-table parser)
          (lem:syntax-scan-region (lem:buffer-start-point buffer)
                                  (lem:buffer-end-point buffer))
          ;; Line 1: "first: 1" - "first" at 0-4, "1" at 7
          (testing "line 1: 'first' is highlighted"
            (ok (has-attribute-at-position-p buffer 1 0))
            (ok (has-attribute-at-position-p buffer 1 4)))
          (testing "line 1: '1' is highlighted"
            (ok (has-attribute-at-position-p buffer 1 7)))
          ;; Line 2: "second: 2" - "second" at 0-5, "2" at 8
          (testing "line 2: 'second' is highlighted"
            (ok (has-attribute-at-position-p buffer 2 0))
            (ok (has-attribute-at-position-p buffer 2 5)))
          (testing "line 2: '2' is highlighted"
            (ok (has-attribute-at-position-p buffer 2 8))))))))

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
          ;; Verify attributes are applied using public API
          (testing "comment line has attributes"
            (ok (has-attribute-at-position-p buffer 1 0)))
          (testing "key-value line has attributes"
            (ok (has-attribute-at-position-p buffer 2 0)))
          (testing "boolean value line has attributes"
            (ok (has-attribute-at-position-p buffer 3 0)))
          (testing "number value line has attributes"
            (ok (has-attribute-at-position-p buffer 4 0))))))))
