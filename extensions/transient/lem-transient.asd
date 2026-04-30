(defsystem "lem-transient"
  :depends-on ("lem/core")
  :components ((:file "transient")
               (:file "keymap")
               (:file "popup")
               (:file "demo"))
  :in-order-to ((test-op (test-op "lem-transient/tests"))))

(defsystem "lem-transient/tests"
  :depends-on ("lem-transient" "rove")
  :components ((:file "tests/main"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))