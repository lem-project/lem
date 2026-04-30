(defsystem "lem-perl-mode"
  :depends-on ("lem/core" "lem-lsp-mode")
  :serial t
  :components ((:file "perl-mode")
               (:file "lsp-config")))

(defsystem "lem-perl-mode/tests"
  :depends-on ("lem-perl-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
