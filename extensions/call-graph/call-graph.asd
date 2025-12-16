(defsystem "call-graph"
  :description "Language-agnostic call graph data structures and provider protocol"
  :author "Lem Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "provider")))
