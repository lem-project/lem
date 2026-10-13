(defpackage :lem-yaml-mode/lsp-config
  (:use :cl :lem-lsp-mode :lem-lsp-base/type))
(in-package :lem-yaml-mode/lsp-config)


(define-language-spec (yaml-spec lem-yaml-mode:yaml-mode)
  :language-id "yaml"
  :root-uri-patterns '()
  :command '("yaml-language-server" "--stdio")
  :install-command "npm install -g yaml-language-server"
  :readme-url "https://github.com/redhat-developer/yaml-language-server"
  :connection-mode :stdio)

(defmethod spec-initialization-options ((spec yaml-spec))
  (make-lsp-map "yaml.format.enable" +true+))
