(defsystem "lem-claude-code"
  :depends-on ("lem/core"
               "frugal-uuid")
  :serial t
  :components ((:file "claude-code-sdk")
               (:file "claude-code")))
