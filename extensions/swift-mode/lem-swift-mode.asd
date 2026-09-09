(defsystem "lem-swift-mode"
  :depends-on ("lem/core" "yason" "lem-lsp-mode")
  :serial t
  :components ((:file "swift-mode") (:file "lsp-config")))
