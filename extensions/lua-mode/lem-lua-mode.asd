(defsystem "lem-lua-mode"
  :depends-on ("lem" "yason")
  :serial t
  :components ((:file "lua-mode")
               (:file "lsp-config")))
