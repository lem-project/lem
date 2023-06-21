(defsystem "lem-lisp-mode"
  :depends-on ("alexandria"
               "trivial-types"
               "usocket"
               "micros"
               "trivia"
               "uiop"
               "lem-lisp-syntax"
               "lem"
               "lem-process"
               "lem-socket-utils")
  :serial t
  :components ((:file "test-api")
               (:file "exporter")
               (:file "errors")
               (:file "swank-protocol")
               (:file "connections")
               (:file "message-dispatcher")
               (:file "ui-mode")
               (:file "grammer")
               (:file "implementation")
               (:file "internal-package")
               (:file "completion")
               (:file "message")
               (:file "file-conversion")
               (:file "lisp-mode")
               (:file "message-definitions")
               (:file "repl")
               (:file "sldb")
	       (:file "hyperspec")
               (:file "inspector")
               (:file "apropos-mode")
               (:file "autodoc")
               (:file "eval")
               (:file "paren-coloring")
               (:file "misc-commands")
               (:file "package-inferred-system")
               (:file "organize-imports")
               (:file "connection-list")
               (:file "self-insert-hook")
               (:file "class-browser")
               (:file "package")))

(defsystem "lem-lisp-mode/v2"
  :depends-on ("lem-lisp-mode")
  :serial t
  :pathname "v2"
  :components ((:file "eval")
               (:file "lsp-config")))
