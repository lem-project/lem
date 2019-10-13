(defsystem "lem-tests"
  :depends-on ("lem" "cl-ansi-text")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/conditions")
               (:file "tests/utilities")
               (:file "tests/lisp-indent"))
  :perform (test-op (o c)
                    (symbol-call :lem-tests '#:run-all-tests)))
