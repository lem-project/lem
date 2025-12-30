(defpackage :lem-clojure-mode/lsp-config
  (:use :cl :lem-lsp-mode :lem-lsp-base/type))

(in-package :lem-clojure-mode/lsp-config)

(define-language-spec (clojure-spec lem-clojure-mode:clojure-mode)
  :language-id "clojure"
  :root-uri-patterns '("deps.edn" "project.clj" "build.boot" "shadow-cljs.edn" ".lsp/config.edn")
  :command '("clojure-lsp")
  :install-command "brew install clojure-lsp/brew/clojure-lsp-native"
  :readme-url "https://clojure-lsp.io/"
  :connection-mode :stdio)
