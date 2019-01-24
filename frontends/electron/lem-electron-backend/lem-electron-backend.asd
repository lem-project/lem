(defsystem "lem-electron-backend"
  :author "cxxxr"
  :license "MIT"
  :description "electron backend for the Lem editor"
  :depends-on ("lem-jsonrpc")
  :serial t
  :components ((:file "main")))
