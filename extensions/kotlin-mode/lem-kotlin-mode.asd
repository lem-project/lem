(defsystem "lem-kotlin-mode"
  :depends-on ("lem/core" "lem-lsp-mode")
  :serial t
  :components ((:file "kotlin-mode")
               (:file "lsp-config")))
