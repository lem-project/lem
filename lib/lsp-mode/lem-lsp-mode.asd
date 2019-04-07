(defsystem "lem-lsp-mode"
  :depends-on ("jsonrpc" "quri" #|"lem-process"|#)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "jsonrpc-util")
               ;(:file "jsonrpc-transport")
               (:file "lsp-mode")))
