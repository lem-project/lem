(defsystem "lem-translator"
  :depends-on ("lem")
  :serial t
  :components ((:file "urlencode")
               (:file "translator")
               (:file "lingva")))