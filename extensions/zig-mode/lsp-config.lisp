(defpackage :lem-zig-mode/lsp-config
  (:use :cl
        :lem-lsp-mode
        :lem-lsp-base/type))
(in-package :lem-zig-mode/lsp-config)

(define-language-spec (zig-spec lem-zig-mode:zig-mode)
  :language-id "zig"
  :root-uri-patterns '("build.zig" "build.zig.zon" "zls.json")
  :command '("zls")
  :install-command "See https://github.com/zigtools/zls for installation instructions"
  :readme-url "https://github.com/zigtools/zls"
  :connection-mode :stdio)
