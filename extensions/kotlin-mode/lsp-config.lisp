(defpackage :lem-kotlin-mode/lsp-config
  (:use :cl
        :lem-lsp-mode
        :lem-lsp-base/type))
(in-package :lem-kotlin-mode/lsp-config)

(define-language-spec (kotlin-spec lem-kotlin-mode:kotlin-mode)
  :language-id "kotlin"
  :root-uri-patterns '("build.gradle" "build.gradle.kts" "settings.gradle" "settings.gradle.kts" "pom.xml")
  :command '("kotlin-language-server")
  :install-command "See https://github.com/fwcd/kotlin-language-server for installation instructions"
  :readme-url "https://github.com/fwcd/kotlin-language-server"
  :connection-mode :stdio)
