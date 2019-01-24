(defsystem "lem-jsonrpc"
  :author "cxxxr"
  :license "MIT"
  :description "jsonrpc backend for the Lem editor"
  :depends-on ("lem"
               "jsonrpc")
  :serial t
  :components ((:file "jsonrpc")))

(defsystem "lem-jsonrpc/executable"
  :author "cxxxr"
  :license "MIT"
  :description "executable entry point for jsonrpc backend for the Lem editor"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem:main"
  :depends-on ("lem-jsonrpc" "lem-electron-backend"))(defsystem "lem-jsonrpc/executable"
  :build-operation program-op
  :build-pathname "lem-rpc"
  :entry-point "lem:main"
  :depends-on ("lem-jsonrpc" "lem-electron-backend"))
