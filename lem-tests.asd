(defsystem "lem-tests"
  :depends-on ("lem-base"
               "lem-core"
               "lem-lsp-utils"
               "lem-fake-interface"
               "lem-lisp-mode"
               "cl-ansi-text"
               "trivial-package-local-nicknames")
  :pathname "tests"
  :components ((:file "deftest")
               (:file "utilities")
               (:module "lsp-utils"
                :components ((:file "json")
                             (:file "json-lsp-utils")))
               (:module "lisp-syntax"
                :components ((:file "indent-test")
                             (:file "defstruct-to-defclass")))
               (:module "lisp-mode"
                :components ((:file "package-inferred-system")))
               (:file "string-width-utils")
               (:file "syntax-test")
               (:file "buffer-list-test")
               (:file "popup-window")
               (:file "isearch")
               (:file "self-insert-command")
               (:file "main"))
  :perform (test-op (o c)
                    (symbol-call :lem-tests :run-all-tests)))
