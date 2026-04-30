(defsystem "lem-git-gutter/diff-parser"
  :description "Git diff parser for lem-git-gutter"
  :depends-on ("cl-ppcre")
  :components ((:file "diff-parser")))

(defsystem "lem-git-gutter"
  :description "Git gutter mode for Lem - shows line changes in the gutter"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("lem/core"
               "lem-git-gutter/diff-parser")
  :serial t
  :components ((:file "git-gutter"))
  :in-order-to ((test-op (test-op "lem-git-gutter/tests"))))

(defsystem "lem-git-gutter/tests"
  :depends-on ("lem-git-gutter" "rove")
  :components ((:module "tests"
                :components ((:file "git-gutter-tests"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
