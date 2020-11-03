(defsystem "lem-lsp-mode"
  :class :package-inferred-system
  :depends-on ("cl-package-locks"
               "trivial-package-local-nicknames"
               "lem-lsp-mode/project"
               "lem-lsp-mode/lsp-mode"))
