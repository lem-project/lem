(defsystem "lem-lisp-mode"
  :depends-on ("alexandria"
               "trivial-types"
               "usocket"
               "swank"
               "trivia"
               "uiop"
               "lem-lisp-syntax"
               "lem"
               "lem-process"
               "lem-socket-utils")
  :serial t
  :components ((:file "test-api")
               (:file "errors")
               (:file "swank-modules")
               (:file "swank-protocol")
               (:file "connections")
               (:file "message-dispatcher")
               (:file "ui-mode")
               (:file "grammer")
               (:file "internal-package")
               (:file "message")
               (:file "file-conversion")
               (:file "lisp-mode")
               (:file "message-definitions")
               (:file "repl")
               (:file "sldb")
               (:file "inspector")
               (:file "apropos-mode")
               (:file "autodoc")
               (:file "paren-coloring")
               (:file "misc-commands")
               (:file "package-inferred-system")
               (:file "organize-imports")
               (:file "connection-list")
               (:file "package")))

(defsystem "lem-lisp-mode/v2"
  :depends-on ("lem-lisp-mode")
  :serial t
  :pathname "v2"
  :components ((:file "eval")
               (:file "lsp-config")))
