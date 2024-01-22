(defsystem "lem-format"
  :depends-on (:lem :lem-c-mode :lem-lisp-mode :lem-go-mode)
  :serial t 
  :components ((:file "format")
               (:file "defaults")))
