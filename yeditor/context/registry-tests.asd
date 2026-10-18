(defsystem "registry-tests"
  :depends-on ("yeditor-context" "yeditor-tx" "rove")
  :components ((:file "registry-test"))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :rove '#:run c)))
