(defsystem "yeditor-effects-t6-tests"
  :description "T6 tests: process, fdefinition effects (integration)"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-effects" "rove")
  :components ((:file "effects-t6-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
