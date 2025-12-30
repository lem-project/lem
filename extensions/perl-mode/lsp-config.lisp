(defpackage :lem-perl-mode/lsp-config
  (:use :cl
        :lem-lsp-mode
        :lem-lsp-base/type))
(in-package :lem-perl-mode/lsp-config)

(define-language-spec (perl-spec lem-perl-mode:perl-mode)
  :language-id "perl"
  :root-uri-patterns '("Makefile.PL" "Build.PL" "cpanfile" "META.json" "META.yml" "dist.ini")
  :command '("pls")
  :install-command "cpanm PLS"
  :readme-url "https://github.com/FractalBoy/perl-language-server"
  :connection-mode :stdio)
