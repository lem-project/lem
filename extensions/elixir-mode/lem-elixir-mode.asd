(defsystem "lem-elixir-mode"
  :depends-on ("lem"
               #+ip-management
               "ip-management")
  :serial t
  :components ((:file "elixir-mode")
	       (:file "lsp-config")
               #+ip-management
               (:file "run-elixir")))
