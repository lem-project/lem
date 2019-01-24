(defsystem "lem-process"
  :author "cxxxr"
  :license "MIT"
  :description "OS processes for the Lem editor"
  :depends-on ("async-process" "lem-core")
  :serial t
  :components ((:file "package")
               (:file "process")
               (:file "stream")))
