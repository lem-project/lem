(defsystem "yeditor-tx-tests"
  :description "Tests for yeditor-tx"
  :depends-on ("yeditor-tx" "rove")
  :components ((:file "tx-test"))
  :perform (asdf:test-op (lambda (op c) (rove:run :yeditor-tx-tests))))
