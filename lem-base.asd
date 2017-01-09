(asdf:defsystem lem-base
  :depends-on (:uiop
               :iterate
               :alexandria
               :cl-ppcre
               :babel)
  :components ((:module "src/base"
                :serial t
                :components ((:file "package")
                             (:file "errors")
                             (:file "attribute")
                             (:file "wide")
                             (:file "macros")
                             (:file "hooks")
                             (:file "line")
                             (:file "buffer")
                             (:file "buffer-insert")
                             (:file "buffers")
                             (:file "point")
                             (:file "overlay")
                             (:file "basic")
                             (:file "syntax")
                             (:file "file")
                             (:file "search")
                             (:file "indent")))))
