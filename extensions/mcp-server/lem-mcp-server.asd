(defsystem "lem-mcp-server"
  :description "MCP (Model Context Protocol) server for Lem editor"
  :version "0.1.0"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "hunchentoot"
               "yason"
               "lem/core")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "protocol")
               (:file "method")
               (:file "server")
               (:file "lifecycle")
               (:file "tools")
               (:file "resources")
               (:module "handlers"
                :serial t
                :components ((:file "buffer")
                             (:file "editing")
                             (:file "command")))
               (:file "commands")))
