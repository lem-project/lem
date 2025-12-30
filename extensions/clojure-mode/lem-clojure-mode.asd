(defsystem "lem-clojure-mode"
  :depends-on ("lem/core"
               "lem-lsp-mode"
               "lem/listener-mode"
               "lem/button"
               "lem/loading-spinner"
               "usocket"
               "babel"
               "bordeaux-threads")
  :serial t
  :components ((:file "clojure-mode")
               (:file "lsp-config")
               (:file "bencode")
               (:file "nrepl-client")
               (:file "repl")
               (:file "commands")
               (:file "inspector")
               (:file "test-runner")
               (:file "stacktrace")))
