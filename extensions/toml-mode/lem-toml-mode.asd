(defsystem "lem-toml-mode"
  :depends-on ("lem/core" "lem-tree-sitter")
  :serial t
  :components ((:file "toml-mode")))

(defsystem "lem-toml-mode/tests"
  :depends-on ("lem-toml-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
