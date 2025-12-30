(defsystem "lem-clojure-mode"
  :depends-on ("lem/core"
               "lem-lsp-mode"
               "lem/listener-mode"
               "usocket"
               "babel"
               "bordeaux-threads")
  :serial t
  :components ((:file "clojure-mode")
               (:file "lsp-config")
               (:file "bencode")
               (:file "nrepl-client")
               (:file "repl")
               (:file "commands")))
