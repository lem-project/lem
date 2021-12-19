(defsystem "lem-lsp-mode"
  :depends-on ("lem-lsp-utils"
               "alexandria"
               "cl-package-locks"
               "jsonrpc"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/tcp"
               "quri"
               "trivia"
               "trivial-package-local-nicknames")
  :serial t
  :components ((:file "project")
               (:file "utils")
               (:file "async-process-stream")
               (:file "lem-stdio-transport")
               (:file "client")
               (:file "request")
               (:file "context-menu")
               (:file "lsp-mode")))
