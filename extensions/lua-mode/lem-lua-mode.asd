(defsystem "lem-lua-mode"
  :depends-on ("lem/core" "yason" "lem-lsp-mode")
  :serial t
  :components ((:file "lua-mode")
               (:file "lsp-config")))
