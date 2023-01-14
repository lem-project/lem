(defsystem "lem-go-mode"
  :depends-on ("lem" "yason")
  :serial t
  :components ((:file "go-mode")
               (:file "lsp-config")))
