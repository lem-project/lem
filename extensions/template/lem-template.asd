(asdf:defsystem "lem-template"
  :author "garlic0x1"
  :license "MIT"
  :description "A system for snippets and new file templates."
  :depends-on (:cl-template)
  :components ((:file "utils")
               (:file "render")
               (:file "prompt")
               (:file "template")
               (:file "snippet")
               (:file "package")))
