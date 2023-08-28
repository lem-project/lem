(defsystem "lem-vi-mode"
  :depends-on ("esrap"
               "closer-mop"
               "lem"
               "cl-ppcre"
               "parse-number"
               "cl-package-locks"
               "alexandria"
               "split-sequence")
  :components ((:file "core")
               (:file "options" :depends-on ("utils"))
               (:file "word" :depends-on ("core" "options"))
               (:file "modeline" :depends-on ("core"))
               (:file "states" :depends-on ("core" "modeline"))
               (:file "visual" :depends-on ("core" "states"))
               (:file "jump-motions")
               (:module "commands-utils"
                :pathname "commands"
                :depends-on ("core" "jump-motions" "visual" "states")
                :components ((:file "utils")))
               (:file "commands" :depends-on ("core" "commands-utils" "word" "visual" "jump-motions" "states"))
               (:file "ex-core")
               (:file "ex-parser" :depends-on ("ex-core"))
               (:file "ex-command" :depends-on ("ex-core" "options" "utils"))
               (:file "ex" :depends-on ("core" "ex-parser"))
               (:file "binds" :depends-on ("states" "commands" "ex" "visual"))
               (:file "special-binds" :depends-on ("core"))
               (:file "vi-mode" :depends-on ("core" "options" "ex" "commands" "states"))
               (:file "utils"))
  :in-order-to ((test-op (test-op "lem-vi-mode/tests"))))

(defsystem "lem-vi-mode/tests"
  :depends-on ("lem"
               "lem-vi-mode"
               "lem-fake-interface"
               "rove"
               "cl-ppcre"
               "cl-interpol"
               "named-readtables"
               "alexandria")
  :components
  ((:module "tests"
    :depends-on ("utils")
    :components
    ((:file "motion")
     (:file "operator")
     (:file "visual")
     (:file "commands")
     (:file "options")))
   (:file "utils"
    :pathname "tests/utils"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
