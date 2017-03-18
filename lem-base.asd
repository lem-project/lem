(asdf:defsystem lem-base
  :depends-on (:uiop
               :iterate
               :alexandria
               :cl-ppcre)
  :components ((:module "src/base"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "errors")
                             (:file "var")
                             (:file "wide")
                             (:file "macros")
                             (:file "hooks")
                             (:file "line")
                             (:file "buffer")
                             (:file "buffer-insert")
                             (:file "buffers")
                             (:file "point")
                             (:file "basic")
                             (:file "syntax")
                             (:file "file")
                             (:file "search")
                             (:file "indent")))))
