(defsystem "lem-tests"
  :depends-on ("lem/core"
               "lem-fake-interface"
               "lem-lisp-syntax"
               "lem-lisp-mode"
               "lem-legit"
               #+sbcl "lem-mcp-server"
               "cl-ansi-text"
               "trivial-package-local-nicknames"
               "rove"
               "yason")
  :pathname "tests"
  :components ((:file "utilities")
               (:module "buffer"
                :components ((:file "internal")))
               (:module "common"
                :components ((:file "ring")
                             (:file "killring")
                             (:file "history")
                             (:file "timer")))
               #+sbcl
               (:module "language-server"
                :components ((:file "utils")
                             (:file "micros-tests")
                             (:file "tests")))
               #+sbcl
               (:module "mcp-server"
                :components ((:file "utils")
                             (:file "integration-tests")
                             (:file "edge-case-tests")))
               (:module "lisp-syntax"
                :components ((:file "indent-test")
                             (:file "defstruct-to-defclass")))
               (:module "lisp-mode"
                :components ((:file "package-inferred-system")))
               (:file "killring")
               (:file "string-width-utils")
               (:file "syntax-test")
               (:file "buffer-list-test")
               (:file "popup-window")
               (:file "prompt")
               (:file "cursors")
               (:file "isearch")
               (:file "self-insert-command")
               (:file "interp")
               (:file "file")
               (:file "scala-mode")
               (:file "completion")
               (:file "command-line-arguments")
               (:file "window")
               (:file "legit"))
  :perform (test-op (o c)
                    (symbol-call :rove :run c)))
