(defpackage :lem-rust-mode/lsp-config
  (:use :cl
        :lem-lsp-mode
        :lem-lsp-base/type))
(in-package :lem-rust-mode/lsp-config)

(define-language-spec (rust-spec lem-rust-mode:rust-mode)
  :language-id "rust"
  :root-uri-patterns '("Cargo.toml")
  :command '("rust-analyzer")
  :install-command "rustup component add rust-analyzer"
  :readme-url "https://github.com/rust-lang/rust-analyzer"
  :connection-mode :stdio)
