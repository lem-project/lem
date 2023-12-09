(defsystem "lem-elixir-mode"
  :depends-on ("lem"
               #+unix
               "ip-management")
  :serial t
  :components ((:file "elixir-mode")
	       (:file "lsp-config")
               #+unix
               (:file "run-elixir")))
