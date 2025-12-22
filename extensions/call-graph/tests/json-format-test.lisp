(defpackage #:call-graph/tests/json-format
  (:use #:cl #:rove #:call-graph))
(in-package #:call-graph/tests/json-format)

;;; Test fixtures

(defun make-test-node ()
  "Create a test graph-node with all fields populated."
  (make-graph-node
   :id "TEST-PKG:FOO"
   :name "FOO"
   :package "TEST-PKG"
   :type :function
   :docstring "A test function"
   :arglist "(x y)"
   :source-file "/path/to/file.lisp"
   :source-location '("/path/to/file.lisp" . 42)))

(defun make-test-node-minimal ()
  "Create a minimal test graph-node with required fields only."
  (make-graph-node
   :id "PKG:BAR"
   :name "BAR"
   :package "PKG"
   :type :macro))

(defun make-test-edge ()
  "Create a test graph-edge."
  (make-graph-edge
   :source "PKG:CALLER"
   :target "PKG:CALLEE"
   :call-type :direct))

(defun make-test-graph ()
  "Create a test call-graph with nodes and edges."
  (let ((graph (make-call-graph)))
    (add-node graph (make-graph-node
                     :id "PKG:MAIN"
                     :name "MAIN"
                     :package "PKG"
                     :type :function
                     :docstring "Entry point"
                     :arglist "()"
                     :source-file "/src/main.lisp"
                     :source-location '("/src/main.lisp" . 10)))
    (add-node graph (make-graph-node
                     :id "PKG:HELPER"
                     :name "HELPER"
                     :package "PKG"
                     :type :function
                     :arglist "(x)"
                     :source-file "/src/main.lisp"
                     :source-location '("/src/main.lisp" . 20)))
    (add-edge graph (make-graph-edge
                     :source "PKG:MAIN"
                     :target "PKG:HELPER"
                     :call-type :direct))
    graph))

;;; graph-node-to-alist tests

(deftest graph-node-to-alist-full
  (testing "converts node with all fields to alist"
    (let* ((node (make-test-node))
           (alist (graph-node-to-alist node)))
      (ok (string= "TEST-PKG:FOO" (cdr (assoc "id" alist :test #'string=))))
      (ok (string= "FOO" (cdr (assoc "name" alist :test #'string=))))
      (ok (string= "TEST-PKG" (cdr (assoc "package" alist :test #'string=))))
      (ok (string= "function" (cdr (assoc "type" alist :test #'string=))))
      (ok (string= "A test function" (cdr (assoc "docstring" alist :test #'string=))))
      (ok (string= "(x y)" (cdr (assoc "arglist" alist :test #'string=))))
      (ok (string= "/path/to/file.lisp" (cdr (assoc "sourceFile" alist :test #'string=))))
      (ok (= 42 (cdr (assoc "sourceLine" alist :test #'string=)))))))

(deftest graph-node-to-alist-minimal
  (testing "converts node with optional fields as null"
    (let* ((node (make-test-node-minimal))
           (alist (graph-node-to-alist node)))
      (ok (string= "PKG:BAR" (cdr (assoc "id" alist :test #'string=))))
      (ok (string= "BAR" (cdr (assoc "name" alist :test #'string=))))
      (ok (string= "PKG" (cdr (assoc "package" alist :test #'string=))))
      (ok (string= "macro" (cdr (assoc "type" alist :test #'string=))))
      (ok (null (cdr (assoc "docstring" alist :test #'string=))))
      (ok (null (cdr (assoc "arglist" alist :test #'string=))))
      (ok (null (cdr (assoc "sourceFile" alist :test #'string=))))
      (ok (null (cdr (assoc "sourceLine" alist :test #'string=)))))))

;;; graph-edge-to-alist tests

(deftest graph-edge-to-alist-converts-all-fields
  (testing "converts edge to alist with all fields"
    (let* ((edge (make-test-edge))
           (alist (graph-edge-to-alist edge)))
      (ok (string= "PKG:CALLER" (cdr (assoc "source" alist :test #'string=))))
      (ok (string= "PKG:CALLEE" (cdr (assoc "target" alist :test #'string=))))
      (ok (string= "direct" (cdr (assoc "callType" alist :test #'string=)))))))

(deftest graph-edge-to-alist-indirect-call
  (testing "converts indirect call type correctly"
    (let* ((edge (make-graph-edge :source "A" :target "B" :call-type :indirect))
           (alist (graph-edge-to-alist edge)))
      (ok (string= "indirect" (cdr (assoc "callType" alist :test #'string=)))))))

;;; call-graph-to-alist tests

(deftest call-graph-to-alist-structure
  (testing "produces alist with nodes and edges keys"
    (let* ((graph (make-test-graph))
           (alist (call-graph-to-alist graph)))
      (ok (assoc "nodes" alist :test #'string=))
      (ok (assoc "edges" alist :test #'string=)))))

(deftest call-graph-to-alist-node-count
  (testing "includes all nodes"
    (let* ((graph (make-test-graph))
           (alist (call-graph-to-alist graph))
           (nodes (cdr (assoc "nodes" alist :test #'string=))))
      (ok (= 2 (length nodes))))))

(deftest call-graph-to-alist-edge-count
  (testing "includes all edges"
    (let* ((graph (make-test-graph))
           (alist (call-graph-to-alist graph))
           (edges (cdr (assoc "edges" alist :test #'string=))))
      (ok (= 1 (length edges))))))

(deftest call-graph-to-alist-empty-graph
  (testing "handles empty graph"
    (let* ((graph (make-call-graph))
           (alist (call-graph-to-alist graph)))
      (ok (null (cdr (assoc "nodes" alist :test #'string=))))
      (ok (null (cdr (assoc "edges" alist :test #'string=)))))))

;;; call-graph-to-json tests

(deftest call-graph-to-json-returns-string
  (testing "returns a JSON string"
    (let* ((graph (make-test-graph))
           (json (with-output-to-string (s)
                   (call-graph-to-json graph s))))
      (ok (stringp json))
      (ok (> (length json) 0)))))

(deftest call-graph-to-json-valid-json
  (testing "produces valid JSON that can be parsed"
    (let* ((graph (make-test-graph))
           (json (with-output-to-string (s)
                   (call-graph-to-json graph s)))
           (parsed (yason:parse json :object-as :alist)))
      (ok (assoc "nodes" parsed :test #'string=))
      (ok (assoc "edges" parsed :test #'string=)))))

;;; alist-to-graph-node tests

(deftest alist-to-graph-node-full
  (testing "converts alist with all fields to node"
    (let* ((alist '(("id" . "TEST:FUNC")
                    ("name" . "FUNC")
                    ("package" . "TEST")
                    ("type" . "function")
                    ("docstring" . "A function")
                    ("arglist" . "(a b)")
                    ("sourceFile" . "/test.lisp")
                    ("sourceLine" . 100)))
           (node (alist-to-graph-node alist)))
      (ok (string= "TEST:FUNC" (graph-node-id node)))
      (ok (string= "FUNC" (graph-node-name node)))
      (ok (string= "TEST" (graph-node-package node)))
      (ok (eq :function (graph-node-type node)))
      (ok (string= "A function" (graph-node-docstring node)))
      (ok (string= "(a b)" (graph-node-arglist node)))
      (ok (string= "/test.lisp" (graph-node-source-file node)))
      (ok (equal '("/test.lisp" . 100) (graph-node-source-location node))))))

(deftest alist-to-graph-node-minimal
  (testing "handles alist with null optional fields"
    (let* ((alist '(("id" . "PKG:X")
                    ("name" . "X")
                    ("package" . "PKG")
                    ("type" . "macro")
                    ("docstring" . nil)
                    ("arglist" . nil)
                    ("sourceFile" . nil)
                    ("sourceLine" . nil)))
           (node (alist-to-graph-node alist)))
      (ok (string= "PKG:X" (graph-node-id node)))
      (ok (eq :macro (graph-node-type node)))
      (ok (null (graph-node-docstring node)))
      (ok (null (graph-node-source-location node))))))

;;; alist-to-graph-edge tests

(deftest alist-to-graph-edge-converts-correctly
  (testing "converts alist to edge"
    (let* ((alist '(("source" . "PKG:A")
                    ("target" . "PKG:B")
                    ("callType" . "direct")))
           (edge (alist-to-graph-edge alist)))
      (ok (string= "PKG:A" (graph-edge-source edge)))
      (ok (string= "PKG:B" (graph-edge-target edge)))
      (ok (eq :direct (graph-edge-call-type edge))))))

(deftest alist-to-graph-edge-indirect-type
  (testing "handles indirect call type"
    (let* ((alist '(("source" . "A")
                    ("target" . "B")
                    ("callType" . "indirect")))
           (edge (alist-to-graph-edge alist)))
      (ok (eq :indirect (graph-edge-call-type edge))))))

;;; json-to-call-graph tests

(deftest json-to-call-graph-parses-structure
  (testing "parses JSON to call-graph"
    (let* ((json "{\"nodes\":[],\"edges\":[]}")
           (graph (json-to-call-graph json)))
      (ok (call-graph-p graph))
      (ok (zerop (hash-table-count (call-graph-nodes graph))))
      (ok (null (call-graph-edges graph))))))

(deftest json-to-call-graph-with-nodes
  (testing "parses nodes from JSON"
    (let* ((json "{\"nodes\":[{\"id\":\"PKG:FOO\",\"name\":\"FOO\",\"package\":\"PKG\",\"type\":\"function\",\"docstring\":null,\"arglist\":null,\"sourceFile\":null,\"sourceLine\":null}],\"edges\":[]}")
           (graph (json-to-call-graph json)))
      (ok (= 1 (hash-table-count (call-graph-nodes graph))))
      (ok (find-node graph "PKG:FOO")))))

(deftest json-to-call-graph-with-edges
  (testing "parses edges from JSON"
    (let* ((json "{\"nodes\":[{\"id\":\"A\",\"name\":\"A\",\"package\":\"P\",\"type\":\"function\",\"docstring\":null,\"arglist\":null,\"sourceFile\":null,\"sourceLine\":null},{\"id\":\"B\",\"name\":\"B\",\"package\":\"P\",\"type\":\"function\",\"docstring\":null,\"arglist\":null,\"sourceFile\":null,\"sourceLine\":null}],\"edges\":[{\"source\":\"A\",\"target\":\"B\",\"callType\":\"direct\"}]}")
           (graph (json-to-call-graph json)))
      (ok (= 1 (length (call-graph-edges graph))))
      (let ((edge (first (call-graph-edges graph))))
        (ok (string= "A" (graph-edge-source edge)))
        (ok (string= "B" (graph-edge-target edge)))))))

;;; Roundtrip tests

(deftest node-roundtrip
  (testing "node survives roundtrip through alist"
    (let* ((original (make-test-node))
           (alist (graph-node-to-alist original))
           (restored (alist-to-graph-node alist)))
      (ok (string= (graph-node-id original) (graph-node-id restored)))
      (ok (string= (graph-node-name original) (graph-node-name restored)))
      (ok (string= (graph-node-package original) (graph-node-package restored)))
      (ok (eq (graph-node-type original) (graph-node-type restored)))
      (ok (equal (graph-node-docstring original) (graph-node-docstring restored)))
      (ok (equal (graph-node-arglist original) (graph-node-arglist restored)))
      (ok (equal (graph-node-source-file original) (graph-node-source-file restored)))
      (ok (equal (graph-node-source-location original) (graph-node-source-location restored))))))

(deftest edge-roundtrip
  (testing "edge survives roundtrip through alist"
    (let* ((original (make-test-edge))
           (alist (graph-edge-to-alist original))
           (restored (alist-to-graph-edge alist)))
      (ok (string= (graph-edge-source original) (graph-edge-source restored)))
      (ok (string= (graph-edge-target original) (graph-edge-target restored)))
      (ok (eq (graph-edge-call-type original) (graph-edge-call-type restored))))))

(deftest graph-roundtrip-json
  (testing "call-graph survives roundtrip through JSON"
    (let* ((original (make-test-graph))
           (json (with-output-to-string (s)
                   (call-graph-to-json original s)))
           (restored (json-to-call-graph json)))
      ;; Check node count
      (ok (= (hash-table-count (call-graph-nodes original))
             (hash-table-count (call-graph-nodes restored))))
      ;; Check edge count
      (ok (= (length (call-graph-edges original))
             (length (call-graph-edges restored))))
      ;; Check specific nodes exist
      (ok (find-node restored "PKG:MAIN"))
      (ok (find-node restored "PKG:HELPER"))
      ;; Check node data preserved
      (let ((main (find-node restored "PKG:MAIN")))
        (ok (string= "MAIN" (graph-node-name main)))
        (ok (string= "Entry point" (graph-node-docstring main)))))))
