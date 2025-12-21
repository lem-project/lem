(uiop:define-package :lem-nix-mode/lsp-config
  (:use :cl)
  (:export))
(in-package :lem-nix-mode/lsp-config)

#+(or)
(lem-lsp-mode:define-language-spec (nix-spec lem-nix-mode:nix-mode)
  :language-id "nix"
  :root-uri-patterns '("flake.nix" "flake.lock" "default.nix" "shell.nix")
  :command '("nil")
  :install-command "nix profile install nixpkgs#nil"
  :readme-url "https://github.com/oxalica/nil"
  :connection-mode :stdio)
