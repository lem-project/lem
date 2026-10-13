(uiop:define-package :lem-odin-mode/lsp-config
  (:use :cl)
  (:export))
(in-package :lem-odin-mode/lsp-config)

(lem-lsp-mode:define-language-spec (odin-spec lem-odin-mode:odin-mode)
  :language-id "odin"
  :root-uri-patterns '("ols.json" "odinfmt.json")
  :command '("ols")
  :install-command "See https://github.com/DanielGavin/ols#installation"
  :readme-url "https://github.com/DanielGavin/ols"
  :connection-mode :stdio)
