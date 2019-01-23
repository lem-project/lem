(defsystem "lem-jsonrpc"
  :author "cxxxr"
  :license "MIT"
  :depends-on ("lem"
               "jsonrpc")
  :serial t
  :components ((:file "jsonrpc")))

(defsystem "lem-jsonrpc/executable"
  :author "cxxxr"
  :license "MIT"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem:main"
  :depends-on ("lem-jsonrpc" "lem-electron-backend"))(defsystem "lem-jsonrpc/executable"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem:main"
  :depends-on ("lem-jsonrpc" "lem-electron-backend"))
