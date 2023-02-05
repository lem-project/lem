(defsystem "lem-language-client"
  :depends-on ("jsonrpc"
               "lem-lsp-base")
  :serial t
  :components ((:file "client")
               (:file "request")))
