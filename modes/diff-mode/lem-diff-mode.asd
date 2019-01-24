(defsystem "lem-diff-mode"
  :author "gos-k <mag4.elan@gmail.com>"
  :license "MIT"
  :description "diff mode for the Lem editor"
  :depends-on ("lem-core"
               "cl-ppcre")
  :serial t
  :components ((:file "diff-mode")))
