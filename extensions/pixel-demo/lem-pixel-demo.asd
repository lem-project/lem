(defsystem "lem-pixel-demo"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "pixel-demo")))

(defsystem "lem-pixel-demo/tests"
  :depends-on ("lem-pixel-demo"
               "lem-fake-interface"
               "rove")
  :pathname "tests"
  :components ((:file "pixel-demo-tests"))
  :perform (test-op (o c)
                    (symbol-call :rove :run c)))
