(defpackage :lem-wat-mode/lsp-config
  (:use :cl))
(in-package :lem-wat-mode/lsp-config)

;; After verifying functionality, the initialization process wasn't working correctly, so it has been commented out.
#+(or)
(lem-lsp-mode:define-language-spec (wat-spec lem-wat-mode:wat-mode)
  :language-id "wat"
  :root-uri-patterns '("*.wat" "*.wast")
  :command '("wat_server")
  :install-command "cargo install wat_server"
  :readme-url "https://github.com/g-plane/wasm-language-tools"
  :connection-mode :stdio)
