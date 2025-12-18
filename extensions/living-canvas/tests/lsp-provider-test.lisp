(defpackage #:lem-living-canvas/tests/lsp-provider
  (:use #:cl #:rove)
  (:local-nicknames (:provider :lem-living-canvas/lsp-provider)
                    (:cg :call-graph)))
(in-package #:lem-living-canvas/tests/lsp-provider)

;;; Provider Class Tests

(deftest provider-instantiation
  (testing "can create lsp-call-hierarchy-provider instance"
    (let ((provider (make-instance 'provider:lsp-call-hierarchy-provider)))
      (ok (typep provider 'cg:call-graph-provider)))))

(deftest provider-name-returns-keyword
  (testing "provider-name returns :lsp-call-hierarchy"
    (let ((provider (make-instance 'provider:lsp-call-hierarchy-provider)))
      (ok (eq :lsp-call-hierarchy (cg:provider-name provider))))))

(deftest provider-priority-higher-than-default
  (testing "provider-priority returns value higher than micros (10)"
    (let ((provider (make-instance 'provider:lsp-call-hierarchy-provider)))
      (ok (> (cg:provider-priority provider) 10)))))

(deftest provider-supports-p-nil-for-non-buffer
  (testing "provider-supports-p returns nil for non-buffer sources"
    (let ((provider (make-instance 'provider:lsp-call-hierarchy-provider)))
      (ng (cg:provider-supports-p provider "/path/to/file.go"))
      (ng (cg:provider-supports-p provider nil))
      (ng (cg:provider-supports-p provider "package-name")))))

(deftest provider-analyze-returns-empty-graph-for-non-buffer
  (testing "provider-analyze returns empty graph for non-buffer sources"
    (let* ((provider (make-instance 'provider:lsp-call-hierarchy-provider))
           (graph (cg:provider-analyze provider "/path/to/file.go")))
      (ok (cg:call-graph-p graph))
      (ok (zerop (hash-table-count (cg:call-graph-nodes graph))))
      (ok (null (cg:call-graph-edges graph))))))

;;; Note: Integration tests with actual LSP servers require:
;;; - A running language server
;;; - A real buffer connected to the LSP server
;;; These would be better suited for manual testing or a dedicated
;;; integration test suite.
