(uiop:define-package #:lem-ansible-mode/lsp-config
  (:use #:cl #:lem-lsp-mode)
  (:import-from :lem-ansible-mode
                :ansible-mode)
  (:import-from :lem-yaml-mode/lsp-config
                #:yaml-spec))

(in-package :lem-ansible-mode/lsp-config)

(define-language-spec (ansible-spec ansible-mode
                                    :parent-spec yaml-spec)
  :language-id "ansible"
  :command '("ansible-language-server" "--stdio")
  :root-uri-patterns '()
  :install-command "npm install -g @ansible/ansible-language-server"
  :readme-url "https://docs.ansible.com/projects/vscode-ansible/als/"
  :connection-mode :stdio)