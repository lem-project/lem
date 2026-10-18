(defsystem "lem-process"
  :depends-on ((:feature :sbcl "sb-posix") "async-process" "lem/core")
  :serial t
  :components ((:file "package")
               (:file "process")
               (:file "stream")))
