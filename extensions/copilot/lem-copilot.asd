(defsystem "lem-copilot"
  :depends-on ("lem" "lem-lsp-mode")
  :components ((:file "utils")
               (:file "logger")
               (:file "client")
               (:file "copilot")))
