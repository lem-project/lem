(defsystem "lem-jsonrpc"
  :depends-on ("lem"
               "lem/extensions"
               "jsonrpc"
               "trivial-utf-8"
               "jsonrpc/transport/stdio")
  :serial t
  :components ((:file "jsonrpc")
               (:file "main")))

(defsystem "lem-jsonrpc/executable"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem:main"
  :depends-on ("lem-jsonrpc" "lem-electron-backend"))
