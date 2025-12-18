(defpackage #:call-graph-lsp/tests/lsp-converter
  (:use #:cl #:rove)
  (:local-nicknames (:lsp :lem-lsp-base/protocol-3-17)))
(in-package #:call-graph-lsp/tests/lsp-converter)

;;; Test fixtures

(defun make-test-range (start-line start-char end-line end-char)
  "Create a test LSP Range object."
  (make-instance 'lsp:range
                 :start (make-instance 'lsp:position
                                       :line start-line
                                       :character start-char)
                 :end (make-instance 'lsp:position
                                     :line end-line
                                     :character end-char)))

(defun make-test-call-hierarchy-item (&key
                                        (name "testFunction")
                                        (kind lsp:symbol-kind-function)
                                        (uri "file:///path/to/test.go")
                                        (detail "(x int) string")
                                        (line 10)
                                        (char 0))
  "Create a test CallHierarchyItem object."
  (make-instance 'lsp:call-hierarchy-item
                 :name name
                 :kind kind
                 :uri uri
                 :detail detail
                 :range (make-test-range line char (+ line 5) 1)
                 :selection-range (make-test-range line char line (+ char (length name)))))

(defun make-test-incoming-call (from-name from-uri from-line)
  "Create a test CallHierarchyIncomingCall object."
  (make-instance 'lsp:call-hierarchy-incoming-call
                 :from (make-test-call-hierarchy-item
                        :name from-name
                        :uri from-uri
                        :line from-line)
                 :from-ranges (vector (make-test-range 15 4 15 20))))

(defun make-test-outgoing-call (to-name to-uri to-line)
  "Create a test CallHierarchyOutgoingCall object."
  (make-instance 'lsp:call-hierarchy-outgoing-call
                 :to (make-test-call-hierarchy-item
                      :name to-name
                      :uri to-uri
                      :line to-line)
                 :from-ranges (vector (make-test-range 12 8 12 24))))

;;; uri-to-filepath tests

(deftest uri-to-filepath-converts-file-uri
  (testing "converts file:// URI to path"
    (ok (string= "/path/to/file.go"
                 (call-graph-lsp:uri-to-filepath "file:///path/to/file.go")))))

(deftest uri-to-filepath-handles-non-file-uri
  (testing "returns non-file URI as-is"
    (ok (string= "https://example.com"
                 (call-graph-lsp:uri-to-filepath "https://example.com")))))

(deftest uri-to-filepath-handles-nil
  (testing "returns nil for nil input"
    (ok (null (call-graph-lsp:uri-to-filepath nil)))))

(deftest uri-to-filepath-handles-short-string
  (testing "returns short strings as-is"
    (ok (string= "file" (call-graph-lsp:uri-to-filepath "file")))))

;;; make-node-id-from-item tests

(deftest make-node-id-from-item-format
  (testing "creates ID with uri:name:line format"
    (let* ((item (make-test-call-hierarchy-item
                  :name "myFunc"
                  :uri "file:///test/file.go"
                  :line 42))
           (id (call-graph-lsp:make-node-id-from-item item)))
      (ok (stringp id))
      (ok (search "file:///test/file.go" id))
      (ok (search "myFunc" id))
      (ok (search "42" id)))))

(deftest make-node-id-from-item-uniqueness
  (testing "different lines produce different IDs"
    (let* ((item1 (make-test-call-hierarchy-item :name "func" :line 10))
           (item2 (make-test-call-hierarchy-item :name "func" :line 20))
           (id1 (call-graph-lsp:make-node-id-from-item item1))
           (id2 (call-graph-lsp:make-node-id-from-item item2)))
      (ng (string= id1 id2)))))

(deftest make-node-id-from-item-same-function-same-line
  (testing "same function at same line produces identical IDs"
    (let* ((item1 (make-test-call-hierarchy-item :name "func" :line 10))
           (item2 (make-test-call-hierarchy-item :name "func" :line 10))
           (id1 (call-graph-lsp:make-node-id-from-item item1))
           (id2 (call-graph-lsp:make-node-id-from-item item2)))
      (ok (string= id1 id2)))))

;;; symbol-kind-to-type-keyword tests

(deftest symbol-kind-to-type-keyword-mappings
  (testing "maps LSP symbol kinds to type keywords"
    (ok (eq :function (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-function)))
    (ok (eq :method (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-method)))
    (ok (eq :constructor (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-constructor)))
    (ok (eq :class (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-class)))
    (ok (eq :interface (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-interface)))
    (ok (eq :module (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-module)))
    (ok (eq :namespace (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-namespace)))
    (ok (eq :property (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-property)))
    (ok (eq :field (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-field)))
    (ok (eq :variable (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-variable)))
    (ok (eq :constant (call-graph-lsp:symbol-kind-to-type-keyword lsp:symbol-kind-constant)))))

(deftest symbol-kind-to-type-keyword-default
  (testing "defaults to :function for unknown kinds"
    (ok (eq :function (call-graph-lsp:symbol-kind-to-type-keyword 999)))))

;;; call-hierarchy-item-to-node-alist tests

(deftest call-hierarchy-item-to-node-alist-basic
  (testing "converts item to alist with required fields"
    (let* ((item (make-test-call-hierarchy-item
                  :name "processData"
                  :uri "file:///src/main.go"
                  :detail "(data []byte) error"
                  :line 25))
           (alist (call-graph-lsp:call-hierarchy-item-to-node-alist item)))
      ;; Check id
      (ok (assoc "id" alist :test #'string=))
      ;; Check name
      (ok (string= "processData" (cdr (assoc "name" alist :test #'string=))))
      ;; Check type
      (ok (string= "function" (cdr (assoc "type" alist :test #'string=))))
      ;; Check arglist (detail)
      (ok (string= "(data []byte) error" (cdr (assoc "arglist" alist :test #'string=))))
      ;; Check sourceFile
      (ok (string= "/src/main.go" (cdr (assoc "sourceFile" alist :test #'string=))))
      ;; Check sourceLine (1-indexed)
      (ok (= 26 (cdr (assoc "sourceLine" alist :test #'string=)))))))

(deftest call-hierarchy-item-to-node-alist-method
  (testing "converts method item with correct type"
    (let* ((item (make-test-call-hierarchy-item
                  :name "MyMethod"
                  :kind lsp:symbol-kind-method))
           (alist (call-graph-lsp:call-hierarchy-item-to-node-alist item)))
      (ok (string= "method" (cdr (assoc "type" alist :test #'string=)))))))

(deftest call-hierarchy-item-to-node-alist-constructor
  (testing "converts constructor item with correct type"
    (let* ((item (make-test-call-hierarchy-item
                  :name "NewService"
                  :kind lsp:symbol-kind-constructor))
           (alist (call-graph-lsp:call-hierarchy-item-to-node-alist item)))
      (ok (string= "constructor" (cdr (assoc "type" alist :test #'string=)))))))

(deftest call-hierarchy-item-to-node-alist-package
  (testing "extracts package from filepath"
    (let* ((item (make-test-call-hierarchy-item
                  :uri "file:///path/to/mypackage.go"))
           (alist (call-graph-lsp:call-hierarchy-item-to-node-alist item)))
      (ok (string= "mypackage" (cdr (assoc "package" alist :test #'string=)))))))

;;; incoming-call-to-edge-alist tests

(deftest incoming-call-to-edge-alist-structure
  (testing "creates edge alist with source, target, callType"
    (let* ((target-item (make-test-call-hierarchy-item :name "targetFunc" :line 50))
           (target-id (call-graph-lsp:make-node-id-from-item target-item))
           (incoming (make-test-incoming-call "callerFunc" "file:///caller.go" 100))
           (edge (call-graph-lsp:incoming-call-to-edge-alist incoming target-id)))
      ;; Check target is the provided ID
      (ok (string= target-id (cdr (assoc "target" edge :test #'string=))))
      ;; Check source is from the incoming call's 'from' item
      (ok (search "callerFunc" (cdr (assoc "source" edge :test #'string=))))
      ;; Check callType
      (ok (string= "direct" (cdr (assoc "callType" edge :test #'string=)))))))

;;; outgoing-call-to-edge-alist tests

(deftest outgoing-call-to-edge-alist-structure
  (testing "creates edge alist with source, target, callType"
    (let* ((source-item (make-test-call-hierarchy-item :name "sourceFunc" :line 30))
           (source-id (call-graph-lsp:make-node-id-from-item source-item))
           (outgoing (make-test-outgoing-call "calleeFunc" "file:///callee.go" 200))
           (edge (call-graph-lsp:outgoing-call-to-edge-alist outgoing source-id)))
      ;; Check source is the provided ID
      (ok (string= source-id (cdr (assoc "source" edge :test #'string=))))
      ;; Check target is from the outgoing call's 'to' item
      (ok (search "calleeFunc" (cdr (assoc "target" edge :test #'string=))))
      ;; Check callType
      (ok (string= "direct" (cdr (assoc "callType" edge :test #'string=)))))))

;;; Integration test

(deftest integration-roundtrip-alist
  (testing "converted alists are compatible with call-graph format"
    (let* ((item (make-test-call-hierarchy-item
                  :name "TestFunc"
                  :uri "file:///test.go"
                  :detail "()"
                  :line 1))
           (node-alist (call-graph-lsp:call-hierarchy-item-to-node-alist item)))
      ;; Verify all required keys for call-graph JSON format are present
      (ok (assoc "id" node-alist :test #'string=))
      (ok (assoc "name" node-alist :test #'string=))
      (ok (assoc "package" node-alist :test #'string=))
      (ok (assoc "type" node-alist :test #'string=))
      (ok (assoc "docstring" node-alist :test #'string=))
      (ok (assoc "arglist" node-alist :test #'string=))
      (ok (assoc "sourceFile" node-alist :test #'string=))
      (ok (assoc "sourceLine" node-alist :test #'string=))
      ;; Can be converted to graph-node
      (let ((graph-node (call-graph:alist-to-graph-node node-alist)))
        (ok (string= "TestFunc" (call-graph:graph-node-name graph-node)))
        (ok (eq :function (call-graph:graph-node-type graph-node)))))))
