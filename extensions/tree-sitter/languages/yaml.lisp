(defpackage :lem-tree-sitter/languages/yaml
  (:use :cl :lem :lem/language-mode)
  (:local-nicknames (:ts :cl-tree-sitter)
                    (:lem-ts :lem-tree-sitter)))
(in-package :lem-tree-sitter/languages/yaml)

;;;; YAML Tree-sitter Support

(defvar *yaml-ts-syntax-table*
  (make-syntax-table
   :space-chars '(#\Space #\Tab #\Newline)
   :paren-pairs '((#\{ . #\})
                  (#\[ . #\]))
   :string-quote-chars '(#\" #\')
   :escape-chars '(#\\)
   :line-comment-string "#")
  "Syntax table for YAML with tree-sitter.")

(defvar *yaml-ts-highlight-query-path*
  (asdf:system-relative-pathname
   :lem-tree-sitter "queries/yaml/highlights.scm")
  "Path to YAML highlight query.")

(defun yaml-ts-mode-setup ()
  "Set up YAML mode with tree-sitter support."
  (when (lem-ts:tree-sitter-available-p)
    ;; Try to load YAML language
    (handler-case
        (progn
          (unless (ts:get-language "yaml")
            (ts:load-language-from-system "yaml"))
          ;; Create and set parser
          (let ((parser (lem-ts:make-treesitter-parser
                         "yaml"
                         :highlight-query-path *yaml-ts-highlight-query-path*)))
            (set-syntax-parser *yaml-ts-syntax-table* parser)))
      (error (e)
        (message "Tree-sitter YAML not available: ~A" e)))))

;; Register language
(lem-ts:register-treesitter-language
 "yaml"
 :highlight-query-path *yaml-ts-highlight-query-path*)

;;;; YAML mode with tree-sitter highlighting

(define-major-mode yaml-ts-mode language-mode
    (:name "YAML-TS"
     :syntax-table *yaml-ts-syntax-table*
     :mode-hook *yaml-ts-mode-hook*)
  (yaml-ts-mode-setup)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'indent-tabs-mode) nil))

;; File type associations for YAML files
(define-file-type ("yaml" "yml") yaml-ts-mode)
