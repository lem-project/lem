(defsystem "lem-tests"
  :depends-on ("lem")
  :serial t
  :components ((:file "tests/packages")
               (:file "tests/indent")))
