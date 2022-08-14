(defsystem "testif"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "package")
               (:file "identifier")
               (:file "test-result")
               (:file "test")
               (:file "test-table")
               (:file "testif"))
  :in-order-to ((test-op (test-op "testif/tests"))))

(defsystem "testif/tests"
  :depends-on ("testif")
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "identifier")
               (:file "tests"))
  :perform (test-op (o c) (symbol-call :testif/tests '#:self-test)))
