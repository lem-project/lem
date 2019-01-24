(defsystem "lem-lsp-mode"
  :author "cxxxr"
  :license "MIT"
  :description "language server protocol mode for the Lem editor"
  :depends-on ("jsonrpc" "quri" "lem-process")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "jsonrpc-util")
               (:file "jsonrpc-transport")
               (:file "lsp-mode")))
