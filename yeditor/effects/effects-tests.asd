(defsystem "yeditor-effects-tests"
  :description "Tests for yeditor-effects"
  :depends-on ("yeditor-effects" "yeditor-tx" "yeditor-context" "rove")
  :components ((:file "effects-tests"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove '#:run :yeditor-effects-tests)))
