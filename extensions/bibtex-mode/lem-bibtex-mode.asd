(defsystem "lem-bibtex-mode"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "bibtex-mode")))

(defsystem "lem-bibtex-mode/tests"
  :depends-on ("lem-bibtex-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
