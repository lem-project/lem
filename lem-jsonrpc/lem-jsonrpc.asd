(defsystem "lem-jsonrpc"
  :depends-on ("lem-core"
               "jsonrpc"
               "plump"
               "clss")
  :serial t
  :components ((:file "jsonrpc")
               (:file "markdown")
               (:file "webview")))
