(defsystem "lem-typst-mode"
  :depends-on ("lem/core"
               "lem-tree-sitter"
               "lem-lsp-mode")
  :serial t
  :components ((:file "typst-mode")
               (:file "lsp-config")
               (:module "preview" 
                :serial t
                :components ((:file "preview")))))

(defsystem "lem-typst-mode/tests"
  :depends-on ("lem-typst-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c)
             ;; symbol-call is the standard ASDF test-op indirection;
             ;; matches the pattern used in lem-tests.asd.
             (symbol-call :rove '#:run c)))



