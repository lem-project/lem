(defsystem "durability-tests"
  :description "T9 tests: effects log, replay, crash recovery, replay benchmark"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-durability" "rove")
  :components ((:file "durability-tests")))
