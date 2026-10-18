(defsystem "yeditor-tx-tests"
  :description "Tests for yeditor-tx"
  :depends-on ("yeditor-tx" "rove")
  :components ((:file "tx-test") (:file "tx2-test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove '#:run :yeditor-tx-tests)))
