(defsystem "lem-odin-mode"
  :depends-on ("lem/core" "lem-lsp-mode")
  :serial t
  :components ((:file "odin-mode")
               (:file "lsp-config")))
