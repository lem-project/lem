(defsystem "lem-lsp-mode"
  :depends-on ("lem-socket-utils"
               "alexandria"
               "cl-package-locks"
               "jsonrpc"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/tcp"
               "quri"
               "trivia"
               "lem-process"
               "lem-language-server")
  :serial t
  :components ((:file "async-process-stream")
               (:file "lem-stdio-transport")
               (:file "client")
               (:file "request")
               (:file "context-menu")
               (:file "lsp-mode")))
