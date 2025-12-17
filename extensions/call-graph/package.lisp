(defpackage #:call-graph
  (:use #:cl)
  (:export
   ;; Data structures
   #:graph-node
   #:make-graph-node
   #:graph-node-id
   #:graph-node-name
   #:graph-node-package
   #:graph-node-type
   #:graph-node-docstring
   #:graph-node-arglist
   #:graph-node-source-location
   #:graph-node-source-file
   #:graph-node-position

   #:graph-edge
   #:make-graph-edge
   #:graph-edge-source
   #:graph-edge-target
   #:graph-edge-call-type

   #:call-graph
   #:call-graph-p
   #:make-call-graph
   #:call-graph-nodes
   #:call-graph-edges
   #:call-graph-root-package

   ;; Utilities
   #:add-node
   #:add-edge
   #:find-node
   #:make-node-id
   #:remove-duplicate-edges

   ;; JSON serialization
   #:graph-to-cytoscape-json

   ;; Provider protocol
   #:call-graph-provider
   #:provider-name
   #:provider-supports-p
   #:provider-analyze
   #:provider-priority))
