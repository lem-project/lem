(defsystem "lem-latex-mode"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "latex-mode")))

(defsystem "lem-latex-mode/tests"
  :depends-on ("lem-latex-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
