(defpackage #:lem-living-canvas/tests/lsp-call-hierarchy
  (:use #:cl #:rove)
  (:local-nicknames (:lsp :lem-lsp-base/protocol-3-17)
                    (:cg-lsp :call-graph-lsp)))
(in-package #:lem-living-canvas/tests/lsp-call-hierarchy)

;;; Tests for lem-living-canvas/lsp-call-hierarchy
;;;
;;; Most of the pure conversion functions have been moved to call-graph-lsp.
;;; Tests for those functions are now in call-graph-lsp/tests/lsp-converter-test.lisp.
;;;
;;; This file tests integration with call-graph-lsp to ensure the living-canvas
;;; module correctly uses the call-graph-lsp package.

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

;;; Integration tests verifying call-graph-lsp functions work as expected
;;; when called from living-canvas context

(deftest call-graph-lsp-make-node-id-format
  (testing "call-graph-lsp:make-node-id-from-item creates ID with expected format"
    (let* ((item (make-test-call-hierarchy-item
                  :name "myFunc"
                  :uri "file:///test/file.go"
                  :line 42))
           (id (cg-lsp:make-node-id-from-item item)))
      (ok (stringp id))
      (ok (search "file:///test/file.go" id))
      (ok (search "myFunc" id))
      (ok (search "42" id)))))

(deftest call-graph-lsp-node-alist-conversion
  (testing "call-graph-lsp:call-hierarchy-item-to-node-alist converts correctly"
    (let* ((item (make-test-call-hierarchy-item
                  :name "processData"
                  :uri "file:///src/main.go"
                  :detail "(data []byte) error"
                  :line 25))
           (alist (cg-lsp:call-hierarchy-item-to-node-alist item)))
      (ok (assoc "id" alist :test #'string=))
      (ok (string= "processData" (cdr (assoc "name" alist :test #'string=))))
      (ok (string= "function" (cdr (assoc "type" alist :test #'string=))))
      (ok (string= "/src/main.go" (cdr (assoc "sourceFile" alist :test #'string=))))
      (ok (= 26 (cdr (assoc "sourceLine" alist :test #'string=)))))))

(deftest call-graph-lsp-edge-alist-conversion
  (testing "call-graph-lsp edge conversion functions work"
    (let* ((source-item (make-test-call-hierarchy-item :name "sourceFunc" :line 30))
           (source-id (cg-lsp:make-node-id-from-item source-item))
           (outgoing (make-test-outgoing-call "calleeFunc" "file:///callee.go" 200))
           (edge (cg-lsp:outgoing-call-to-edge-alist outgoing source-id)))
      (ok (string= source-id (cdr (assoc "source" edge :test #'string=))))
      (ok (search "calleeFunc" (cdr (assoc "target" edge :test #'string=))))
      (ok (string= "direct" (cdr (assoc "callType" edge :test #'string=)))))))

(deftest call-graph-lsp-build-graph-integration
  (testing "call-graph-lsp:build-call-graph-from-hierarchy builds complete graph"
    (let* ((item (make-test-call-hierarchy-item
                  :name "main"
                  :uri "file:///main.go"
                  :line 10))
           (outgoing-map (make-hash-table :test 'equal)))
      ;; Setup mock outgoing calls
      (setf (gethash "main" outgoing-map)
            '(("helper" "file:///main.go" 30)))
      (let ((graph (cg-lsp:build-call-graph-from-hierarchy
                    (list item)
                    (constantly nil)
                    (lambda (item)
                      (let ((calls (gethash (lsp:call-hierarchy-item-name item) outgoing-map)))
                        (when calls
                          (mapcar (lambda (spec)
                                    (destructuring-bind (name uri line) spec
                                      (make-test-outgoing-call name uri line)))
                                  calls))))
                    :include-outgoing t)))
        ;; Should have 2 nodes: main, helper
        (ok (= 2 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 1 edge: main->helper
        (ok (= 1 (length (call-graph:call-graph-edges graph))))))))
