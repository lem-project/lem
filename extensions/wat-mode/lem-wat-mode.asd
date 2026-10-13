(defsystem "lem-wat-mode"
  :depends-on ("lem/core"
               "lem-tree-sitter"
               "lem-lisp-syntax"
               "lem-lsp-mode")
  :serial t
  :components ((:file "indent")
               (:file "wat-mode")
               (:file "lsp-config")
               (:module "tree-sitter"
                :components ((:static-file "highlights.scm")))))
