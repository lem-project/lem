(defsystem "yeditor-effects-t5-tests"
  :description "T5 tests: keymap, variable, command, overlay, timer effects"
  :depends-on ("lem" "yeditor-tx" "yeditor-context" "yeditor-effects" "rove")
  :components ((:file "effects-t5-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
