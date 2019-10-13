(defsystem "lem-tests"
  :depends-on ("lem"
               "rove")
  :serial t
  :components ((:module "tests"
                :components ((:file "packages")
                             (:file "indent")))))
