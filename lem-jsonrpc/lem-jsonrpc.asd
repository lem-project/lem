(defsystem "lem-jsonrpc"
  :depends-on ("lem-core" "jsonrpc")
  :serial t
  :components ((:file "jsonrpc")
               (:file "markdown")
               (:file "webview")))
