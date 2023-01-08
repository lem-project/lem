(defsystem "lem-lsp-mode"
  :depends-on ("lem-lsp-utils"
               "lem-socket-utils"
               "alexandria"
               "cl-package-locks"
               "jsonrpc"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/tcp"
               "quri"
               "trivia")
  :serial t
  :components ((:file "utils")
               (:file "async-process-stream")
               (:file "lem-stdio-transport")
               (:file "client")
               (:file "request")
               (:file "context-menu")
               (:file "lsp-mode")))
