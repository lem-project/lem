(defsystem "lem-jsonrpc"
  :depends-on ("lem"
               "jsonrpc")
  :serial t
  :components ((:file "jsonrpc")))
