(defsystem "lem-process"
  :author "cxxxr"
  :license "MIT"
  :depends-on ("async-process" "lem-core")
  :serial t
  :components ((:file "package")
               (:file "process")
               (:file "stream")))
