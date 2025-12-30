(defsystem "lem-clojure-mode"
  :depends-on ("lem/core"
               "lem-lsp-mode")
  :serial t
  :components ((:file "clojure-mode")
               (:file "lsp-config")))
