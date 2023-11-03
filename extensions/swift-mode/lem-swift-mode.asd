(defsystem "lem-swift-mode"
  :depends-on ("lem" "yason")
  :serial t
  :components ((:file "swift-mode") (:file "lsp-config")))