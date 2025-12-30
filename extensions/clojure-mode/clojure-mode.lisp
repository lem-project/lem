(defpackage :lem-clojure-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*clojure-mode-hook*
           :clojure-mode
           :*clojure-syntax-table*
           :*clojure-mode-keymap*))

(in-package :lem-clojure-mode)

(defvar *clojure-special-forms*
  '("def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "defstruct" "defonce"
    "fn" "fn*" "let" "let*" "letfn" "loop" "recur"
    "if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-some"
    "when-first" "cond" "condp" "case" "cond->" "cond->>" "some->" "some->>"
    "do" "doto" "doseq" "dotimes" "for" "while"
    "try" "catch" "finally" "throw"
    "new" "set!" "quote" "var" "import" "require" "use" "refer"
    "ns" "in-ns" "refer-clojure"
    "binding" "with-open" "with-local-vars" "with-redefs" "with-bindings"
    "lazy-seq" "lazy-cat" "delay" "future" "promise"
    "extend" "extend-type" "extend-protocol" "reify" "proxy"
    "gen-class" "gen-interface"
    "and" "or" "not" "assert"
    "apply" "partial" "comp" "complement" "constantly" "identity"
    "juxt" "memoize" "fnil" "every-pred" "some-fn"))

(defvar *clojure-builtin-functions*
  '("+" "-" "*" "/" "=" "==" "not=" "<" ">" "<=" ">=" "compare"
    "inc" "dec" "quot" "rem" "mod" "max" "min" "abs"
    "nil?" "true?" "false?" "some?" "any?" "even?" "odd?" "zero?" "pos?" "neg?"
    "number?" "integer?" "float?" "rational?" "string?" "keyword?" "symbol?"
    "seq?" "vector?" "list?" "map?" "set?" "coll?" "fn?" "ifn?"
    "first" "second" "rest" "next" "last" "butlast" "nth" "take" "drop"
    "take-while" "drop-while" "take-last" "drop-last" "take-nth"
    "cons" "conj" "concat" "into" "empty" "not-empty"
    "count" "get" "get-in" "assoc" "assoc-in" "dissoc" "update" "update-in"
    "contains?" "keys" "vals" "find" "select-keys"
    "map" "mapv" "mapcat" "filter" "filterv" "remove" "reduce" "reductions"
    "partition" "partition-by" "partition-all" "group-by" "frequencies"
    "sort" "sort-by" "reverse" "shuffle" "flatten" "distinct" "dedupe"
    "interleave" "interpose" "zipmap" "split-at" "split-with"
    "str" "subs" "format" "pr" "prn" "print" "println" "pr-str" "prn-str"
    "name" "namespace" "keyword" "symbol" "gensym"
    "atom" "deref" "reset!" "swap!" "compare-and-set!"
    "ref" "dosync" "ref-set" "alter" "commute"
    "agent" "send" "send-off" "await" "await-for"
    "list" "vector" "vec" "hash-map" "array-map" "sorted-map" "hash-set" "sorted-set"
    "range" "repeat" "repeatedly" "iterate" "cycle" "seq"))

(defvar *clojure-constants*
  '("nil" "true" "false"))

(defun tokens (boundary strings)
  "Create a regex pattern from a list of strings with optional word boundary."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-clojure ()
  "Create TextMate-style language definition for Clojure."
  (let* ((patterns (make-tm-patterns
                    ;; Line comment
                    (make-tm-line-comment-region ";")
                    ;; Shebang comment (for scripts)
                    (make-tm-line-comment-region "#!")
                    ;; Discard reader macro (comment next form)
                    (make-tm-match "#_"
                                   :name 'syntax-comment-attribute)
                    ;; Keywords (:keyword)
                    (make-tm-match ":[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-constant-attribute)
                    ;; Namespaced keywords (::keyword, :ns/keyword)
                    (make-tm-match "::[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match ":[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*/[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-constant-attribute)
                    ;; Strings
                    (make-tm-string-region "\"")
                    ;; Regex literals
                    (make-tm-region "#\"" "\""
                                    :name 'syntax-string-attribute)
                    ;; Character literals
                    (make-tm-match "\\\\(?:newline|space|tab|backspace|formfeed|return|u[0-9a-fA-F]{4}|o[0-7]{1,3}|.)"
                                   :name 'syntax-string-attribute)
                    ;; Numeric literals (integers, floats, ratios, hex, octal, radix)
                    (make-tm-match "\\b[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?[MN]?\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b0[xX][0-9a-fA-F]+N?\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b[0-9]+/[0-9]+\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Constants (nil, true, false)
                    (make-tm-match (tokens :word-boundary *clojure-constants*)
                                   :name 'syntax-constant-attribute)
                    ;; Special forms and macros
                    (make-tm-match (tokens :word-boundary *clojure-special-forms*)
                                   :name 'syntax-keyword-attribute)
                    ;; Built-in functions
                    (make-tm-match (tokens :word-boundary *clojure-builtin-functions*)
                                   :name 'syntax-builtin-attribute)
                    ;; Metadata reader macro
                    (make-tm-match "\\^[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-type-attribute)
                    ;; Deref reader macro
                    (make-tm-match "@[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-variable-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *clojure-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\- #\* #\+ #\! #\' #\? #\< #\> #\= #\/)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")
                :line-comment-string ";"))
        (tmlanguage (make-tmlanguage-clojure)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode clojure-mode language-mode
    (:name "Clojure"
     :keymap *clojure-mode-keymap*
     :syntax-table *clojure-syntax-table*
     :mode-hook *clojure-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) ";"
        (variable-value 'insertion-line-comment) ";; "
        (variable-value 'beginning-of-defun-function) 'clojure-beginning-of-defun
        (variable-value 'end-of-defun-function) 'clojure-end-of-defun))

(defun clojure-beginning-of-defun (point n)
  "Move to the beginning of a defun in Clojure code."
  (loop :repeat n
        :do (search-backward-regexp point "^\\s*\\((?:def|defn|defn-|defmacro|defmulti|defmethod|defprotocol|defrecord|deftype|ns)\\b")))

(defun clojure-end-of-defun (point n)
  "Move to the end of a defun in Clojure code."
  (with-point ((p point))
    (loop :repeat n
          :do (clojure-beginning-of-defun p -1)
              (unless (form-offset p 1) (return)))
    (move-point point p)))

(define-file-type ("clj" "cljs" "cljc" "cljx" "edn") clojure-mode)
