(defsystem "lem-living-canvas"
  :description "Living Canvas - Visual function graph editor for Lem"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("call-graph"
               "alexandria"
               "lem/core"
               "lem-lisp-mode"
               "yason")
  :serial t
  :components ((:file "micros-cl-provider")
               (:file "buffer")
               (:file "living-canvas")))
