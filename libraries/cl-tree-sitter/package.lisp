(uiop:define-package :tree-sitter/ffi
  (:use :cl)
  (:export
   ;; Library
   #:tree-sitter-available-p
   #:ensure-tree-sitter-loaded
   #:ensure-ts-wrapper-loaded

   ;; Parser
   #:ts-parser-new
   #:ts-parser-delete
   #:ts-parser-set-language
   #:ts-parser-parse-string
   #:ts-parser-parse
   #:ts-parser-reset

   ;; Tree
   #:ts-tree-delete
   #:ts-tree-copy
   #:ts-tree-root-node
   #:ts-tree-edit
   #:ts-tree-language

   ;; Node (raw)
   #:ts-node-type
   #:ts-node-symbol
   #:ts-node-start-byte
   #:ts-node-end-byte
   #:ts-node-start-point
   #:ts-node-end-point
   #:ts-node-child-count
   #:ts-node-named-child-count
   #:ts-node-child
   #:ts-node-named-child
   #:ts-node-parent
   #:ts-node-next-sibling
   #:ts-node-prev-sibling
   #:ts-node-next-named-sibling
   #:ts-node-prev-named-sibling
   #:ts-node-is-null
   #:ts-node-is-named
   #:ts-node-is-missing
   #:ts-node-is-extra
   #:ts-node-has-error
   #:ts-node-string

   ;; Tree Cursor
   #:ts-tree-cursor-new
   #:ts-tree-cursor-delete
   #:ts-tree-cursor-reset
   #:ts-tree-cursor-current-node
   #:ts-tree-cursor-current-field-name
   #:ts-tree-cursor-goto-parent
   #:ts-tree-cursor-goto-next-sibling
   #:ts-tree-cursor-goto-first-child

   ;; Query
   #:ts-query-new
   #:ts-query-delete
   #:ts-query-pattern-count
   #:ts-query-capture-count
   #:ts-query-capture-name-for-id

   ;; Query Cursor
   #:ts-query-cursor-new
   #:ts-query-cursor-delete
   #:ts-query-cursor-exec
   #:ts-query-cursor-next-match
   #:ts-query-cursor-next-capture
   #:ts-query-cursor-set-byte-range
   #:ts-query-cursor-set-point-range

   ;; Language
   #:ts-language-version
   #:ts-language-symbol-count
   #:ts-language-symbol-name

   ;; Structs
   #:ts-point
   #:ts-point-row
   #:ts-point-column
   #:ts-input-edit
   #:ts-query-match
   #:ts-query-capture
   #:ts-node-raw
   #:ts-tree-cursor))

(uiop:define-package :tree-sitter/types
  (:use :cl)
  (:local-nicknames (:ffi :tree-sitter/ffi))
  (:export
   ;; Classes
   #:ts-parser
   #:ts-tree
   #:ts-node
   #:ts-query
   #:ts-query-cursor
   #:ts-language

   ;; Accessors
   #:ts-parser-ptr
   #:ts-parser-language
   #:ts-tree-ptr
   #:ts-tree-source
   #:ts-node-tree
   #:ts-node-buffer
   #:ts-query-ptr
   #:ts-query-language
   #:ts-query-capture-names
   #:ts-language-ptr
   #:ts-language-name

   ;; Structures
   #:query-match
   #:make-query-match
   #:query-match-pattern-index
   #:query-match-captures
   #:query-capture
   #:make-query-capture
   #:query-capture-node
   #:query-capture-index
   #:query-capture-name
   #:ts-point
   #:make-ts-point
   #:ts-point-row
   #:ts-point-column
   #:ts-input-edit
   #:make-ts-input-edit
   #:ts-input-edit-start-byte
   #:ts-input-edit-old-end-byte
   #:ts-input-edit-new-end-byte
   #:ts-input-edit-start-point
   #:ts-input-edit-old-end-point
   #:ts-input-edit-new-end-point))

(uiop:define-package :tree-sitter/parser
  (:use :cl)
  (:local-nicknames (:ffi :tree-sitter/ffi)
                    (:types :tree-sitter/types))
  (:export
   #:make-parser
   #:parser-set-language
   #:parser-parse-string
   #:parser-parse
   #:parser-reset
   #:parser-delete
   #:with-parser))

(uiop:define-package :tree-sitter/node
  (:use :cl)
  (:local-nicknames (:ffi :tree-sitter/ffi)
                    (:types :tree-sitter/types))
  (:export
   ;; Tree operations
   #:tree-root-node
   #:tree-edit
   #:tree-copy

   ;; Node accessors
   #:node-type
   #:node-symbol
   #:node-start-byte
   #:node-end-byte
   #:node-start-point
   #:node-end-point
   #:node-range
   #:node-child-count
   #:node-named-child-count
   #:node-child
   #:node-named-child
   #:node-children
   #:node-named-children
   #:node-parent
   #:node-next-sibling
   #:node-prev-sibling
   #:node-next-named-sibling
   #:node-prev-named-sibling
   #:node-null-p
   #:node-named-p
   #:node-missing-p
   #:node-extra-p
   #:node-has-error-p
   #:node-string

   ;; Tree cursor
   #:with-tree-cursor
   #:cursor-current-node
   #:cursor-current-field-name
   #:cursor-goto-parent
   #:cursor-goto-next-sibling
   #:cursor-goto-first-child

   ;; Point
   #:make-point
   #:point-row
   #:point-column

   ;; Edit
   #:make-input-edit))

(uiop:define-package :tree-sitter/query
  (:use :cl)
  (:local-nicknames (:ffi :tree-sitter/ffi)
                    (:types :tree-sitter/types)
                    (:node :tree-sitter/node))
  (:export
   #:query-compile
   #:query-delete
   #:query-pattern-count
   #:query-capture-count
   #:get-capture-name-by-index

   #:with-query-cursor
   #:query-exec
   #:query-matches
   #:query-captures
   #:query-captures-in-range

   ;; Match/Capture accessors
   #:match-pattern-index
   #:match-captures
   #:capture-node
   #:capture-index
   #:capture-name))

(uiop:define-package :tree-sitter/language
  (:use :cl)
  (:local-nicknames (:ffi :tree-sitter/ffi)
                    (:types :tree-sitter/types))
  (:export
   #:load-language
   #:load-language-from-system
   #:register-language
   #:get-language
   #:list-languages
   #:language-version
   #:language-symbol-count
   #:language-symbol-name))

(uiop:define-package :tree-sitter
  (:use :cl)
  (:nicknames :ts)
  (:import-from :tree-sitter/ffi
   #:tree-sitter-available-p)
  (:use-reexport
   :tree-sitter/types
   :tree-sitter/parser
   :tree-sitter/node
   :tree-sitter/query
   :tree-sitter/language)
  (:export
   #:tree-sitter-available-p))
