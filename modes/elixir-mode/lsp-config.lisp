(defpackage :lem-elixir-mode/lsp-config
  (:use :cl
         :lem-lsp-mode
         :lem-lsp-base/type))

(in-package :lem-elixir-mode/lsp-config)

;;TODO: Add this variable to the documentation on how to use lsp with elixir-mode
(defvar *server-path* "language_server.sh")

(define-language-spec (elixir-spec lem-elixir-mode:elixir-mode)
  :language-id "elixir"
  :root-uri-patterns '("mix.exs")
  :command `("sh" ,*server-path*)
  :install-command ""
  :readme-url "https://github.com/elixir-lsp/elixir-ls"
  :connection-mode :stdio)
