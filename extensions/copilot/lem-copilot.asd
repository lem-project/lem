(defsystem "lem-copilot"
  :depends-on ("lem" "lem-lsp-mode")
  :components ((:file "internal")
               (:file "copilot")))
