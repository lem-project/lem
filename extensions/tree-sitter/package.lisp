(uiop:define-package :lem-tree-sitter/highlight
  (:use :cl)
  (:local-nicknames (:ts :tree-sitter))
  (:export
   #:capture-to-attribute
   #:make-default-capture-attribute-map
   #:load-highlight-query))

(uiop:define-package :lem-tree-sitter
  (:use :cl :lem)
  (:local-nicknames (:ts :tree-sitter)
                    (:highlight :lem-tree-sitter/highlight))
  (:export
   ;; Parser class
   #:treesitter-parser
   #:make-treesitter-parser
   #:treesitter-parser-language-name

   ;; Enable tree-sitter for existing modes
   #:enable-tree-sitter-for-mode
   #:enable-tree-sitter-for-all-modes

   ;; Utilities
   #:tree-sitter-available-p))
