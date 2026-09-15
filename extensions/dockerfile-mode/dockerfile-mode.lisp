
(defpackage :lem-dockerfile-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*dockerfile-mode-hook*
   :dockerfile-mode
           :*dockerfile-syntax-table*
   :*dockerfile-mode-keymap*))

(in-package :lem-dockerfile-mode)


(defparameter *docker-keywords*
  '("FROM" "RUN" "CMD" "LABEL" "MAINTAINER" "EXPOSE" "ENV" "ADD" "COPY"
    "ENTRYPOINT" "VOLUME" "USER" "WORKDIR" "ARG" "ONBUILD" "STOPSIGNAL"
    "HEALTHCHECK" "SHELL" "AS" "CROSS_BUILD"
    "from" "maintainer" "run" "cmd" "expose" "env" "arg"
    "add" "copy" "entrypoint" "volume" "user" "workdir" "onbuild"
    "label" "stopsignal" "shell" "healthcheck" "as" "cross_build" ))


(defvar *dockerfile-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\- #\$ #\/ #\. #\: #\= #\@)
                :paren-pairs '((#\( . #\))
                               (#\[ . #\])
                               (#\{ . #\}))
                :string-quote-chars '(#\" #\')
                :escape-chars '(#\\)
                :expr-prefix-chars '(#\$)
                :line-comment-string "#"
                :block-comment-pairs nil))
        (tmlanguage (make-tmlanguage-dockerfile)))
    (set-syntax-parser table tmlanguage)
    table)
  "Syntax table for Dockerfile mode.")


(defun tokens (boundary strings)
  "Create a regex alternation pattern from STRINGS, optionally wrapped with BOUNDARY."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))




(defun make-tmlanguage-dockerfile ()
  "Create parser for dockerfile syntax"
  (let* ((patterns
           (make-tm-patterns
            ;; comments
            (make-tm-line-comment-region "#")

            ;; strings
            (make-tm-string-region "\"")
            (make-tm-string-region "'")
            
            ;; commands / keywords
            (make-tm-match (tokens :word-boundary *docker-keywords*) :name 'syntax-keyword-attribute)

            ;; build flags
            (make-tm-match "--[a-zA-Z0-9_-]+(=[^\\s]+)?"
                           :name 'syntax-variable-attribute)

            ;; variables $VAR or ${VAR}
            (make-tm-match "\\$[a-zA-Z_][a-zA-Z0-9_]*|\\$\\{[a-zA-Z_][a-zA-Z0-9_:-]*\\}"
                           :name 'syntax-variable-attribute)

            ;; line continuation
            (make-tm-match "\\\\$"
                           :name 'syntax-escape-attribute))))



    (make-tmlanguage :patterns patterns)))



(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for dockerfile."
  (asdf:system-relative-pathname :lem-dockerfile-mode "tree-sitter/highlights.scm"))

(define-major-mode dockerfile-mode language-mode
  (:name "Dockerfile"
   :keymap *dockerfile-mode-keymap*
   :syntax-table *dockerfile-syntax-table*
   :mode-hook *dockerfile-mode-hook*)
  "Major mode for writing/modifying dockerfiles"
  (let ((query-path (tree-sitter-query-path)))
    (when (and query-path (probe-file query-path))
      (lem-tree-sitter:enable-tree-sitter-for-mode
       *dockerfile-syntax-table*
       "dockerfile"
       query-path)))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "#"
        (variable-value 'insertion-line-comment) "# "))


(define-file-type ("dockerfile" "Dockerfile" "containerfile") dockerfile-mode)
(define-file-associations dockerfile-mode
    ((:file-namestring "Dockerfile")
     (:file-namestring "dockerfile")
     (:file-namestring "containerfile")
     (:file-namestring "Containerfile")))
