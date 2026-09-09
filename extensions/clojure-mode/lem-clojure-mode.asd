(defsystem "lem-clojure-mode"
  :depends-on ("lem/core"
               "lem-lsp-mode"
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
               (:file "stacktrace")
               (:file "detective")
               (:file "tools"))
  :in-order-to ((test-op (test-op "lem-clojure-mode/tests"))))

(defsystem "lem-clojure-mode/tests"
  :depends-on ("lem/core"
               "lem-clojure-mode"
               "rove")
  :components
  ((:module "tests"
    :components
    ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
