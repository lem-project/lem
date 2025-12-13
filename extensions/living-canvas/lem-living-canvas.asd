(defsystem "lem-living-canvas"
  :description "Living Canvas - Visual function graph editor for Lem"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("alexandria"
               "lem/core"
               "lem-server"
               "yason")
  :serial t
  :components ((:file "package")
               (:file "call-graph")
               (:file "canvas-buffer")
               (:file "commands")))
