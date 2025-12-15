(defsystem "lem-living-canvas"
  :description "Living Canvas - Visual function graph editor for Lem"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("alexandria"
               "lem/core"
               "yason")
  :serial t
  :components ((:file "call-graph")
               (:file "graph-format")
               (:file "buffer")
               (:file "living-canvas")))
