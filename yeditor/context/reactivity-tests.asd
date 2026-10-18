(defsystem "yeditor-context-reactivity-tests"
  :description "T7 tests: coeffect classification, context-event dispatch, work-cycle predicates"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-reactivity" "rove")
  :components ((:file "reactivity-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
