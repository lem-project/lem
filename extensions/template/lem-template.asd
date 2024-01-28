(asdf:defsystem "lem-template"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cl-template)
  :components ((:module "src"
                :components ((:file "core")))))
