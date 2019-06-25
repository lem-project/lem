(defsystem "lem-lsp-mode"
  :depends-on ("jsonrpc" "quri" "lem-core" #|"lem-process"|#
               "jsonrpc/transport/tcp")
  :serial t
  :components ((:file "package")
               (:file "jsonrpc")
               (:file "util")
               (:file "jsonrpc-util")
               ;(:file "jsonrpc-transport")
               (:file "lsp-mode")))
