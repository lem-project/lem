(defsystem "lem-jsonrpc"
  :depends-on ("lem"
               "lem/extensions"
               "jsonrpc"
               "trivial-utf-8"
               "jsonrpc/transport/stdio"
               "command-line-arguments")
  :serial t
  :components ((:file "jsonrpc-stdio-patch")
               (:file "utils")
               (:file "view")
               (:file "main")))

(defsystem "lem-jsonrpc/executable"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem-jsonrpc:program"
  :depends-on ("lem-jsonrpc"))
