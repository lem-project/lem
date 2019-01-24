(defsystem "lem-c-mode"
  :author "cxxxr"
  :license "MIT"
  :description "c mode for the Lem editor"
  :depends-on ("lem-core")
  :serial t
  :components ((:file "grammer")
               (:file "c-mode")))
