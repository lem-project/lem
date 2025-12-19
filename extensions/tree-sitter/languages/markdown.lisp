(defpackage :lem-tree-sitter/languages/markdown
  (:use :cl :lem :lem/language-mode)
  (:local-nicknames (:ts :cl-tree-sitter)
                    (:lem-ts :lem-tree-sitter)))
(in-package :lem-tree-sitter/languages/markdown)

;;;; Markdown Tree-sitter Support

(defvar *markdown-ts-syntax-table*
  (make-syntax-table
   :space-chars '(#\Space #\Tab #\Newline)
   :paren-pairs '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\`)
   :escape-chars '(#\\))
  "Syntax table for Markdown with tree-sitter.")

(defvar *markdown-ts-highlight-query-path*
  (asdf:system-relative-pathname
   :lem-tree-sitter "queries/markdown/highlights.scm")
  "Path to Markdown highlight query.")

(defun markdown-ts-mode-setup ()
  "Set up Markdown mode with tree-sitter support."
  (when (lem-ts:tree-sitter-available-p)
    ;; Try to load Markdown language
    (handler-case
        (progn
          (unless (ts:get-language "markdown")
            (ts:load-language-from-system "markdown"))
          ;; Create and set parser
          (let ((parser (lem-ts:make-treesitter-parser
                         "markdown"
                         :highlight-query-path *markdown-ts-highlight-query-path*)))
            (set-syntax-parser *markdown-ts-syntax-table* parser)))
      (error (e)
        (message "Tree-sitter Markdown not available: ~A" e)))))

;;;; Markdown mode with tree-sitter highlighting

(define-major-mode markdown-ts-mode language-mode
    (:name "Markdown-TS"
     :syntax-table *markdown-ts-syntax-table*
     :mode-hook *markdown-ts-mode-hook*)
  (markdown-ts-mode-setup)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'indent-tabs-mode) nil))

;; File type associations for markdown files
(define-file-type ("md" "markdown" "mkd" "mkdn" "mdown" "mdwn") markdown-ts-mode)
