(defsystem "lem-process"
  :depends-on ("async-process" "lem-core")
  :serial t
  :components ((:file "process")))
