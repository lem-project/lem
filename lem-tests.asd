(pushnew :lem-tests.rove *features*)
;; (pushnew :lem-tests.fiveam *features*)

(defsystem "lem-tests"
  :depends-on ("lem-base"
               "lem"
               "lem-lsp-utils"
               "lem-lsp-server"
               "lem-fake-interface"
               "lem-lisp-syntax"
               "lem-lisp-mode"
               "cl-ansi-text"
               "trivial-package-local-nicknames"
               #+lem-tests.rove "rove"
               #+lem-tests.fiveam "fiveam")
  :pathname "tests"
  :components ((:module "test-if"
                :components (#+lem-tests.rove (:file "rove")
                             #+lem-tests.fiveam (:file "fiveam")))
               (:file "utilities")
               (:module "common"
                :components ((:file "ring")))
               (:module "lsp-utils"
                :components ((:file "json")
                             (:file "json-lsp-utils")))
               (:module "lsp-server"
                :components ((:file "test-server")
                             (:file "initialize")
                             (:file "initialized")
                             (:file "text-document-did-open")))
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
                    (symbol-call :lem-tests/test-if :run-all-tests)))
