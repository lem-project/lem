(defpackage :lem-living-canvas/call-graph
  (:use :cl)
  (:export #:graph-node
           #:make-graph-node
           #:graph-node-id
           #:graph-node-name
           #:graph-node-package
           #:graph-node-type
           #:graph-node-docstring
           #:graph-node-source-location
           #:graph-node-source-file
           #:graph-node-position
           #:graph-edge
           #:make-graph-edge
           #:graph-edge-source
           #:graph-edge-target
           #:graph-edge-call-type
           #:call-graph
           #:make-call-graph
           #:call-graph-nodes
           #:call-graph-edges
           #:call-graph-root-package
           #:analyze-package
           #:analyze-file
           #:analyze-buffer
           #:analyze-system
           #:graph-to-cytoscape-json))

(defpackage :lem-living-canvas/buffer
  (:use :cl :lem)
  (:import-from :lem-living-canvas/call-graph
                #:call-graph
                #:graph-to-cytoscape-json
                #:analyze-buffer)
  (:export #:canvas-buffer
           #:make-canvas-buffer
           #:canvas-buffer-graph
           #:canvas-buffer-source-buffer
           #:canvas-buffer-node-positions
           #:update-canvas-buffer))

(defpackage :lem-living-canvas
  (:use :cl :lem)
  (:import-from :lem-living-canvas/call-graph
                #:analyze-package
                #:analyze-file
                #:analyze-buffer
                #:analyze-system
                #:graph-to-cytoscape-json)
  (:import-from :lem-living-canvas/buffer
                #:canvas-buffer
                #:make-canvas-buffer
                #:canvas-buffer-graph
                #:canvas-buffer-source-buffer)
  (:export #:living-canvas
           #:living-canvas-current-file
           #:living-canvas-system
           #:living-canvas-refresh))
