(uiop:define-package :lem-typst-mode/lsp-config
  (:use :cl))
(in-package :lem-typst-mode/lsp-config)

(lem-lsp-mode:define-language-spec (typst-spec lem-typst-mode:typst-mode)
  :language-id "typst"
  :root-uri-patterns '("typst.toml" ".git")
  :command '("tinymist" "lsp")
  :install-command "cargo install --git https://github.com/Myriad-Dreamin/tinymist --locked tinymist-cli"
  :readme-url "https://github.com/Myriad-Dreamin/tinymist"
  :connection-mode :stdio)