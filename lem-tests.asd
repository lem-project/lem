(defsystem "lem-tests"
  :class :package-inferred-system
  :depends-on ("lem-base"
               "lem-core"
               "lem-tests/main")
  :pathname "tests"
  :perform (test-op (o c)
                    (symbol-call :lem-tests :run-all-tests)))

(asdf:register-system-packages "lem-lisp-syntax" '(:lem-lisp-syntax.defstruct-to-defclass))
(asdf:register-system-packages "lem-lisp-mode" '(:lem-lisp-mode.package-inferred-system))
