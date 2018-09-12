(defsystem "lem-electron-backend"
  :depends-on ("lem-jsonrpc")
  :serial t
  :components ((:file "main")))
