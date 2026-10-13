(uiop:define-package :lem-dockerfile-mode/lsp-config
  (:use :cl))

(in-package :lem-dockerfile-mode/lsp-config)

(lem-lsp-mode:define-language-spec (dockerfile-spec lem-dockerfile-mode:dockerfile-mode)
  :language-id "dockerfile"
  :root-uri-patterns '(".git" "Dockerfile" "Containerfile" "dockerfile" "containerfile")
  :command '("docker-langserver" "--stdio")
  :install-command "npm install -g dockerfile-language-server-nodejs"
  :readme-url "https://github.com/rcjsuen/dockerfile-language-server"
  :connection-mode :stdio)

