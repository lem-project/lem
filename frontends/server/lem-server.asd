(defsystem "lem-server"
  :depends-on ("lem/core"
               "lem/extensions"
               "jsonrpc"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/websocket"
               #-os-windows "jsonrpc/transport/local-domain-socket"
               "command-line-arguments")
  :serial t
  :components ((:file "jsonrpc-stdio-patch")
               (:file "config")
               (:file "utils")
               (:file "view")
               (:file "mouse")
               (:file "icon")
               (:file "main")
               (:file "tabbar")
               (:file "color-picker")))
