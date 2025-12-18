(defpackage :lem-tree-sitter/languages/json
  (:use :cl :lem :lem/language-mode)
  (:local-nicknames (:ts :cl-tree-sitter)
                    (:lem-ts :lem-tree-sitter)))
(in-package :lem-tree-sitter/languages/json)

;;;; JSON Tree-sitter Support

(defvar *json-ts-syntax-table*
  (make-syntax-table
   :space-chars '(#\Space #\Tab #\Newline)
   :paren-pairs '((#\{ . #\})
                  (#\[ . #\]))
   :string-quote-chars '(#\")
   :escape-chars '(#\\))
  "Syntax table for JSON with tree-sitter.")

(defvar *json-ts-highlight-query-path*
  (asdf:system-relative-pathname
   :lem-tree-sitter "queries/json/highlights.scm")
  "Path to JSON highlight query.")

(defun json-ts-mode-setup ()
  "Set up JSON mode with tree-sitter support."
  (when (lem-ts:tree-sitter-available-p)
    ;; Try to load JSON language
    (handler-case
        (progn
          (unless (ts:get-language "json")
            (ts:load-language-from-system "json"))
          ;; Create and set parser
          (let ((parser (lem-ts:make-treesitter-parser
                         "json"
                         :highlight-query-path *json-ts-highlight-query-path*)))
            (set-syntax-parser *json-ts-syntax-table* parser)))
      (error (e)
        (message "Tree-sitter JSON not available: ~A" e)))))

;; Register language
(lem-ts:register-treesitter-language
 "json"
 :highlight-query-path *json-ts-highlight-query-path*)

;;;; Provide as an alternative to lem-json-mode
;;; Users can choose to use tree-sitter based highlighting

(define-major-mode json-ts-mode language-mode
    (:name "JSON-TS"
     :syntax-table *json-ts-syntax-table*
     :mode-hook *json-ts-mode-hook*)
  (json-ts-mode-setup)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'indent-tabs-mode) nil))

;; Optional: Define file association
;; (define-file-type ("json" "jsonc" "json5") json-ts-mode)
