(defsystem "loader-tests"
  :description "T8 tests: config loader, reconciliation, delta application"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-loader" "rove")
  :components ((:file "loader-tests")))
