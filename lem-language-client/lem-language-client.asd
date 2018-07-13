(defsystem "lem-language-client"
  :depends-on ("lem"
               "jsonrpc")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "language-client")))
