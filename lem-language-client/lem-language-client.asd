(defsystem "lem-language-client"
  :depends-on ("jsonrpc" "quri")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "jsonrpc")
               (:file "language-client")))
