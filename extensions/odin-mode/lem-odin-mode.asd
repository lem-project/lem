(defsystem "lem-odin-mode"
  :depends-on ("lem/core" "lem-lsp-mode")
  :serial t
  :components ((:file "odin-mode")
               (:file "lsp-config")))

(defsystem "lem-odin-mode/tests"
  :depends-on ("lem-odin-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
