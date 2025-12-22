(defpackage #:call-graph-lsp
  (:use #:cl)
  (:local-nicknames (:lsp :lem-lsp-base/protocol-3-17))
  (:export
   ;; LSP→alist conversion
   #:uri-to-filepath
   #:make-node-id-from-item
   #:symbol-kind-to-type-keyword
   #:call-hierarchy-item-to-node-alist
   #:incoming-call-to-edge-alist
   #:outgoing-call-to-edge-alist
   ;; Graph building
   #:build-call-graph-from-hierarchy))
(in-package #:call-graph-lsp)
