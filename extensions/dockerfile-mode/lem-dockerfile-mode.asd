(defsystem "lem-dockerfile-mode"
  :depends-on ("lem/core"
               "lem-tree-sitter"
               "lem-lsp-mode")
  :serial t
  :components ((:file "dockerfile-mode")
               (:file "lsp-config")))


(defsystem "lem-dockerfile-mode/tests"
  :depends-on ("lem-dockerfile-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))