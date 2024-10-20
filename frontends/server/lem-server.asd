(defsystem "lem-server"
  :depends-on ("lem"
               "lem/extensions"
               "jsonrpc"
               "trivial-utf-8"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/websocket"
               "jsonrpc/transport/local-domain-socket"
               "command-line-arguments"
               "ningle"
               "com.inuoe.jzon"
               "cl-ini")
  :serial t
  :components ((:file "jsonrpc-stdio-patch")
               (:file "config")
               (:file "utils")
               (:file "view")
               (:file "mouse")
               (:file "main")))
