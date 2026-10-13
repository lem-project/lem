(defsystem "lem-zig-mode"
  :depends-on ("lem/core" "lem-lsp-mode")
  :serial t
  :components ((:file "zig-mode")
               (:file "lsp-config")))
