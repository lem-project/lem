(uiop:define-package :lem-tree-sitter/highlight
  (:use :cl)
  (:local-nicknames (:ts :cl-tree-sitter))
  (:export
   #:*capture-attribute-map*
   #:capture-to-attribute
   #:define-capture-mapping
   #:load-highlight-query))

(uiop:define-package :lem-tree-sitter
  (:use :cl :lem)
  (:local-nicknames (:ts :cl-tree-sitter)
                    (:highlight :lem-tree-sitter/highlight))
  (:export
   ;; Parser class
   #:treesitter-parser
   #:make-treesitter-parser
   #:treesitter-parser-language-name

   ;; Utilities
   #:tree-sitter-available-p))
