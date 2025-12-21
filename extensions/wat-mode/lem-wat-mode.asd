(defsystem "lem-wat-mode"
  :depends-on ("lem/core"
               "lem-tree-sitter"
               "lem-lisp-syntax"
               "lem-lsp-mode")
  :serial t
  :components ((:file "indent")
               (:file "wat-mode")
               (:file "lsp-config")
               (:module "tree-sitter"
                :components ((:static-file "highlights.scm"))))
  :in-order-to ((test-op (test-op "lem-wat-mode/tests"))))

(defsystem "lem-wat-mode/tests"
  :depends-on ("lem-wat-mode" "rove")
  :serial t
  :pathname "tests"
  :components ((:file "main"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
