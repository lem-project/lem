(uiop:define-package #:lem-python-mode/lsp-config
  (:use #:cl)
  (:export))
(in-package :lem-python-mode/lsp-config)

(lem-lsp-mode:define-language-spec (python-spec lem-python-mode:python-mode)
  :language-id "python"
  :root-uri-patterns '("setup.py" "pyproject.toml" "requirements.txt" "poetry.lock")
  :command '("pylsp")
  :install-command "pip install python-lsp-server"
  :readme-url "https://github.com/python-lsp/python-lsp-server"
  :connection-mode :stdio)
