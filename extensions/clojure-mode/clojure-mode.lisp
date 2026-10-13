(defpackage :lem-clojure-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*clojure-mode-hook*
           :clojure-mode
           :*clojure-syntax-table*
           :*clojure-mode-keymap*
           ;; Indentation
           :clojure-calc-indent
           :clojure-set-indentation
           :clojure-get-indentation
           ;; Namespace
           :clojure-current-namespace
           :clojure-guess-namespace
           ;; Rainbow parentheses
           :*clojure-paren-coloring*
           :toggle-clojure-paren-coloring))

(in-package :lem-clojure-mode)

;;;; Syntax Keywords

(defvar *clojure-special-forms*
  '("def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "defstruct" "defonce" "definline" "definterface"
    "fn" "fn*" "let" "let*" "letfn" "loop" "recur"
    "if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-some"
    "when-first" "cond" "condp" "case" "cond->" "cond->>" "some->" "some->>"
    "as->" "do" "doto" "doseq" "dotimes" "for" "while"
    "try" "catch" "finally" "throw"
    "new" "set!" "quote" "var" "import" "require" "use" "refer"
    "ns" "in-ns" "refer-clojure" "create-ns"
    "binding" "with-open" "with-local-vars" "with-redefs" "with-bindings"
    "with-out-str" "with-in-str" "with-precision" "with-meta"
    "lazy-seq" "lazy-cat" "delay" "future" "promise" "pvalues" "pcalls"
    "extend" "extend-type" "extend-protocol" "reify" "proxy" "deftype"
    "gen-class" "gen-interface"
    "and" "or" "not" "assert"
    "apply" "partial" "comp" "complement" "constantly" "identity"
    "juxt" "memoize" "fnil" "every-pred" "some-fn"
    "->" "->>" "doto" ".."))

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
    "contains?" "keys" "vals" "find" "select-keys" "merge" "merge-with"
    "map" "mapv" "mapcat" "filter" "filterv" "remove" "reduce" "reductions"
    "partition" "partition-by" "partition-all" "group-by" "frequencies"
    "sort" "sort-by" "reverse" "shuffle" "flatten" "distinct" "dedupe"
    "interleave" "interpose" "zipmap" "split-at" "split-with"
    "str" "subs" "format" "pr" "prn" "print" "println" "pr-str" "prn-str"
    "name" "namespace" "keyword" "symbol" "gensym"
    "atom" "deref" "reset!" "swap!" "compare-and-set!" "add-watch" "remove-watch"
    "ref" "dosync" "ref-set" "alter" "commute" "ensure"
    "agent" "send" "send-off" "await" "await-for"
    "list" "vector" "vec" "hash-map" "array-map" "sorted-map" "hash-set" "sorted-set"
    "range" "repeat" "repeatedly" "iterate" "cycle" "seq" "lazy-seq"
    "re-pattern" "re-matches" "re-find" "re-seq" "re-groups"
    "slurp" "spit" "read" "read-string" "eval" "load" "load-file"
    "type" "class" "instance?" "isa?" "ancestors" "descendants" "parents"
    "meta" "with-meta" "vary-meta" "alter-meta!" "reset-meta!"))

(defvar *clojure-constants*
  '("nil" "true" "false"))

;;;; Indentation System

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defvar *clojure-indent-table* (make-hash-table :test 'equal))

(defun clojure-get-indentation (name)
  "Get indentation method for NAME."
  (gethash name *clojure-indent-table*))

(defun clojure-set-indentation (name method)
  "Set indentation METHOD for NAME."
  (setf (gethash name *clojure-indent-table*) method))

;; Initialize Clojure indentation rules
(mapc (lambda (elt)
        (let ((name (car elt))
              (method (if (stringp (cdr elt))
                          (clojure-get-indentation (cdr elt))
                          (cadr elt))))
          (clojure-set-indentation name method)))
      '(;; Core special forms with body
        ("fn" (&lambda &body))
        ("defn" (4 &lambda &body))
        ("defn-" . "defn")
        ("defmacro" . "defn")
        ("defmulti" (4 &lambda &body))
        ("defmethod" (4 4 &lambda &body))
        ("defprotocol" (4 &rest (&whole 2 &rest 1)))
        ("defrecord" (4 (&whole 4 &rest 1) &rest (&whole 2 &rest 1)))
        ("deftype" . "defrecord")
        ("definterface" (4 &rest (&whole 2 &rest 1)))
        ("reify" (&rest (&whole 2 &lambda &body)))
        ("proxy" (4 4 &rest (&whole 2 &lambda &body)))
        ("extend-type" (4 &rest (&whole 2 &lambda &body)))
        ("extend-protocol" (4 &rest (&whole 2 &lambda &body)))

        ;; Binding forms
        ("let" ((&whole 4 &rest (&whole 1 2)) &body))
        ("letfn" ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
        ("if-let" . "let")
        ("if-some" . "let")
        ("when-let" . "let")
        ("when-some" . "let")
        ("when-first" . "let")
        ("loop" . "let")
        ("binding" . "let")
        ("with-open" . "let")
        ("with-local-vars" . "let")
        ("with-redefs" . "let")
        ("with-bindings" . "let")
        ("doseq" . "let")
        ("dotimes" . "let")
        ("for" . "let")

        ;; Conditionals
        ("if" (&rest nil))
        ("if-not" . "if")
        ("when" 1)
        ("when-not" . "when")
        ("cond" (&rest (&whole 2 &rest 1)))
        ("condp" (4 4 &rest (&whole 2 &rest 1)))
        ("case" (4 &rest (&whole 2 &rest 1)))

        ;; Threading macros - align arguments
        ("->" 1)
        ("->>" 1)
        ("some->" 1)
        ("some->>" 1)
        ("cond->" 1)
        ("cond->>" 1)
        ("as->" (4 4 &body))

        ;; Other special forms
        ("do" (&rest &body))
        ("doto" (4 &body))
        ("try" (&rest &body))
        ("catch" (4 4 &body))
        ("finally" (&rest &body))
        ("throw" 1)

        ;; ns form
        ("ns" (4 &rest (&whole 2 &rest 1)))

        ;; Java interop
        ("." (4 4 &body))
        (".." 1)
        ("new" (4 &body))

        ;; Misc
        ("comment" (&rest &body))
        ("declare" (&rest 1))
        ("future" (&rest &body))
        ("delay" (&rest &body))
        ("lazy-seq" (&rest &body))
        ("lazy-cat" (&rest &body))
        ("dosync" (&rest &body))
        ("locking" (4 &body))
        ("assert" 1)

        ;; with-* macros
        ("with-out-str" (&rest &body))
        ("with-in-str" (4 &body))
        ("with-precision" (4 &body))
        ("with-meta" (4 &body))))

(defun clojure-compute-indent-integer-method (method path indent-point sexp-column)
  (declare (ignore indent-point))
  (cond ((cdr path)
         'default-indent)
        ((<= (car path) method)
         (+ sexp-column 4))
        (t
         (+ sexp-column *body-indent*))))

(defun clojure-compute-indent-symbol-method (method path indent-point sexp-column)
  (funcall method path indent-point sexp-column))

(defun clojure-compute-indent-complex-method (method path indent-point sexp-column)
  (loop
    :named exit
    :for pathrest :on path
    :for n := (1- (car pathrest))
    :do (let ((restp nil))
          (loop
            (let ((method1 (car method)))
              (cond ((and restp
                          (not (or (consp method1)
                                   (and (symbolp method1)
                                        (not (member method1 '(&rest &body &whole &lambda)))))))
                     (return-from exit 'default-indent))
                    ((eq method1 '&body)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column *body-indent*)
                           'default-indent)))
                    ((eq method1 '&rest)
                     (setf restp (> n 0))
                     (setf n 0)
                     (pop method))
                    ((> n 0)
                     (decf n)
                     (pop method))
                    ((eq method1 'nil)
                     (return-from exit 'default-indent))
                    ((eq method1 '&lambda)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column 4)
                           (+ sexp-column 2))))
                    ((integerp method1)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column method1)
                           'default-indent)))
                    ((symbolp method1)
                     (return-from exit
                       (clojure-compute-indent-symbol-method method1 path indent-point sexp-column)))
                    ((not (null (cdr pathrest)))
                     (setf method (cddr method1))
                     (return))
                    (t
                     (return-from exit
                       (let ((method1 (cadr method1)))
                         (cond (restp 'default-indent)
                               ((eq method1 'nil) 'default-indent)
                               ((integerp method1) (+ sexp-column method1))
                               (t (clojure-compute-indent-symbol-method
                                   method1 path indent-point sexp-column))))))))))))

(defun clojure-compute-indent-method (method path indent-point sexp-column)
  (funcall (etypecase method
             (integer #'clojure-compute-indent-integer-method)
             (symbol #'clojure-compute-indent-symbol-method)
             (function #'clojure-compute-indent-symbol-method)
             (list #'clojure-compute-indent-complex-method))
           method path indent-point sexp-column))

(defun clojure-quote-form-point-p (p)
  (and (eql (character-at p -1) #\')
       (not (eql (character-at p -2) #\#))))

(defun clojure-vector-form-point-p (p)
  (or (eql (character-at p -1) #\#)
      (eql (character-at p 0) #\[)))

(defun clojure-find-indent-method (name path)
  (flet ((f (method)
           (when method
             (return-from clojure-find-indent-method method))))
    (f (clojure-get-indentation name))
    (let ((name1 (ppcre:scan-to-strings "(?<=/)[^/]+" name)))
      (when name1
        (f (clojure-get-indentation name1)))
      ;; Default rule for def*, with-*, do* forms
      (f (and (null (cdr path))
              (ppcre:scan "^(?:with-|def|do[a-z])" (or name1 name))
              '(&lambda &body))))))

(defun clojure-calc-function-indent (point)
  (loop
    (unless (form-offset point -1)
      (let ((charpos (point-charpos point)))
        (form-offset point 1)
        (skip-whitespace-forward point t)
        (when (or (eql #\; (character-at point))
                  (end-line-p point))
          (line-offset point 0 charpos)))
      (return))
    (let ((charpos (point-charpos point)))
      (back-to-indentation point)
      (when (= charpos (point-charpos point))
        (return))
      (line-offset point 0 charpos)))
  (point-column point))

(defun clojure-calc-indent-1 (indent-point)
  (let* ((const-flag nil)
         (innermost-sexp-column nil)
         (calculated
           (with-point ((p indent-point))
             (loop
               :named outer
               :with path := '() :and sexp-column
               :for innermost := t :then nil
               :repeat *max-depth*
               :do
                  (loop :for n :from 0 :do
                           (when (and (< 0 n) (start-line-p p))
                             (return-from outer nil))
                           (unless (form-offset p -1)
                             (push n path)
                             (return)))
                  (when (and (null (cdr path))
                             (= 0 (car path))
                             (scan-lists p -1 1 t))
                    (return-from outer (1+ (point-column p))))
                  (when (and innermost
                             (or (member (character-at p 0) '(#\: #\"))
                                 (looking-at p "#!?[+-]")))
                    (setf const-flag t))
                  (let ((name (string-downcase (symbol-string-at-point p))))
                    (unless (scan-lists p -1 1 t)
                      (return-from outer 'default-indent))
                    (unless sexp-column (setf sexp-column (point-column p)))
                    (when innermost
                      (setf innermost-sexp-column sexp-column))
                    (when (or (clojure-quote-form-point-p p)
                              (clojure-vector-form-point-p p))
                      (return-from outer (1+ sexp-column)))
                    (let ((method (clojure-find-indent-method name path)))
                      (when method
                        (return-from outer
                          (cond ((eq method 'default-indent)
                                 (setq const-flag nil)
                                 method)
                                (t
                                 (clojure-compute-indent-method method
                                                                path
                                                                indent-point
                                                                sexp-column)))))))))))
    (cond ((and (eq calculated 'default-indent)
                (not const-flag))
           (clojure-calc-function-indent indent-point))
          ((and (or (null calculated)
                    (eq calculated 'default-indent))
                const-flag)
           (1+ innermost-sexp-column))
          (calculated
           (if (eq calculated 'default-indent)
               (clojure-calc-function-indent indent-point)
               calculated))
          (t
           (clojure-calc-function-indent indent-point)))))

(defun clojure-calc-indent (point)
  "Calculate indentation for the current line in Clojure code."
  (line-start point)
  (with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ((pps-state-string-p state) nil)
        ((zerop (pps-state-paren-depth state)) 0)
        (t (clojure-calc-indent-1 point))))))

;;;; Namespace Detection

(defun clojure-guess-namespace (point)
  "Guess the current namespace by finding the (ns ...) form."
  (with-point ((p point))
    (buffer-start p)
    (loop
      (unless (search-forward-regexp p "^\\s*\\(ns\\s+" nil)
        (return nil))
      (let ((state (syntax-ppss p)))
        (unless (or (pps-state-string-p state)
                    (pps-state-comment-p state))
          ;; Found ns form, extract namespace name
          (multiple-value-bind (match groups)
              (looking-at p "([^\\s\\)]+)")
            (declare (ignore groups))
            (when match
              (return (ppcre:regex-replace-all
                       "[\\(\\)]"
                       match
                       "")))))))))

(defun clojure-current-namespace ()
  "Get the current namespace for the buffer."
  (or (buffer-value (current-buffer) 'clojure-namespace)
      (let ((ns (clojure-guess-namespace (current-point))))
        (when ns
          (setf (buffer-value (current-buffer) 'clojure-namespace) ns))
        ns)))

;;;; Rainbow Parentheses

(define-editor-variable clojure-paren-coloring nil
  "Enable rainbow parentheses coloring for Clojure mode."
  (lambda (value)
    (if value
        (clojure-enable-paren-coloring)
        (clojure-disable-paren-coloring))))

(define-attribute clojure-paren-color-1
  (t :foreground "#e06c75"))  ; red

(define-attribute clojure-paren-color-2
  (t :foreground "#61afef"))  ; blue

(define-attribute clojure-paren-color-3
  (t :foreground "#98c379"))  ; green

(define-attribute clojure-paren-color-4
  (t :foreground "#d19a66"))  ; orange

(define-attribute clojure-paren-color-5
  (t :foreground "#c678dd"))  ; purple

(define-attribute clojure-paren-color-6
  (t :foreground "#56b6c2"))  ; cyan

(defparameter *clojure-rainbow-colors*
  #(clojure-paren-color-1
    clojure-paren-color-2
    clojure-paren-color-3
    clojure-paren-color-4
    clojure-paren-color-5
    clojure-paren-color-6))

(defun clojure-paren-coloring (start end)
  "Apply rainbow coloring to parentheses in Clojure code."
  (when (eq 'clojure-mode (buffer-major-mode (point-buffer start)))
    (with-point ((p start)
                 (start-point start)
                 (end-point end))
      (line-start p)
      (line-end end-point)
      (let* ((table (or (buffer-value p 'clojure-coloring-table)
                        (setf (buffer-value p 'clojure-coloring-table)
                              (make-hash-table))))
             (depth (gethash (line-number-at-point p) table 0)))
        (loop :while (point< p end-point)
              :do (let ((attr (text-property-at p :attribute)))
                    (if (member attr '(syntax-comment-attribute syntax-string-attribute))
                        (character-offset p 1)
                        (let ((c (character-at p)))
                          (case c
                            ((#\( #\[ #\{)
                             (move-point start-point p)
                             (character-offset p 1)
                             (put-text-property
                              start-point p
                              :attribute (aref *clojure-rainbow-colors*
                                               (mod depth (length *clojure-rainbow-colors*))))
                             (incf depth))
                            ((#\) #\] #\})
                             (decf depth)
                             (move-point start-point p)
                             (character-offset p 1)
                             (put-text-property
                              start-point p
                              :attribute (aref *clojure-rainbow-colors*
                                               (mod depth (length *clojure-rainbow-colors*)))))
                            (#\newline
                             (setf (gethash (1+ (line-number-at-point p)) table) depth)
                             (character-offset p 1))
                            (#\\
                             (character-offset p 2))
                            (otherwise
                             (character-offset p 1)))))))))))

(defun clojure-enable-paren-coloring ()
  (add-hook (variable-value 'after-syntax-scan-hook :global)
            'clojure-paren-coloring))

(defun clojure-disable-paren-coloring ()
  (remove-hook (variable-value 'after-syntax-scan-hook :global)
               'clojure-paren-coloring))

(define-command toggle-clojure-paren-coloring () ()
  "Toggle rainbow parentheses coloring for Clojure mode."
  (setf (variable-value 'clojure-paren-coloring :global)
        (not (variable-value 'clojure-paren-coloring :global))))

;;;; Syntax Highlighting

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
                    ;; Discard reader macro
                    (make-tm-match "#_"
                                   :name 'syntax-comment-attribute)
                    ;; Keywords (:keyword, ::keyword, :ns/keyword)
                    (make-tm-match "::?[a-zA-Z][a-zA-Z0-9*+!_'?<>=./-]*"
                                   :name 'syntax-constant-attribute)
                    ;; Strings
                    (make-tm-string-region "\"")
                    ;; Regex literals
                    (make-tm-region "#\"" "\""
                                    :name 'syntax-string-attribute)
                    ;; Character literals
                    (make-tm-match "\\\\(?:newline|space|tab|backspace|formfeed|return|u[0-9a-fA-F]{4}|o[0-7]{1,3}|.)"
                                   :name 'syntax-string-attribute)
                    ;; Numeric literals
                    (make-tm-match "\\b[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?[MN]?\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b0[xX][0-9a-fA-F]+N?\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b[0-9]+/[0-9]+\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Constants
                    (make-tm-match (tokens :word-boundary *clojure-constants*)
                                   :name 'syntax-constant-attribute)
                    ;; Special forms
                    (make-tm-match (tokens :word-boundary *clojure-special-forms*)
                                   :name 'syntax-keyword-attribute)
                    ;; Built-in functions
                    (make-tm-match (tokens :word-boundary *clojure-builtin-functions*)
                                   :name 'syntax-builtin-attribute)
                    ;; Metadata
                    (make-tm-match "\\^[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-type-attribute)
                    ;; Deref
                    (make-tm-match "@[a-zA-Z][a-zA-Z0-9*+!_'?<>=/-]*"
                                   :name 'syntax-variable-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *clojure-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\- #\* #\+ #\! #\' #\? #\< #\> #\= #\/ #\.)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")
                :line-comment-string ";"))
        (tmlanguage (make-tmlanguage-clojure)))
    (set-syntax-parser table tmlanguage)
    table))

;;;; Mode Definition

(define-major-mode clojure-mode language-mode
    (:name "Clojure"
     :keymap *clojure-mode-keymap*
     :syntax-table *clojure-syntax-table*
     :mode-hook *clojure-mode-hook*)
  (modeline-add-status-list 'clojure-mode-line (current-buffer))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) ";"
        (variable-value 'insertion-line-comment) ";; "
        (variable-value 'calc-indent-function) 'clojure-calc-indent
        (variable-value 'beginning-of-defun-function) 'clojure-beginning-of-defun
        (variable-value 'end-of-defun-function) 'clojure-end-of-defun))

(defmethod convert-modeline-element ((element (eql 'clojure-mode-line)) window)
  (let ((ns (clojure-current-namespace)))
    (if ns
        (format nil " [~A]" ns)
        "")))

;;;; Navigation

(defun clojure-beginning-of-defun (point n)
  "Move to the beginning of a defun in Clojure code."
  (loop :repeat n
        :do (search-backward-regexp
             point
             "^\\s*\\((?:def[a-z-]*|ns)\\s")))

(defun clojure-end-of-defun (point n)
  "Move to the end of a defun in Clojure code."
  (with-point ((p point))
    (loop :repeat n
          :do (clojure-beginning-of-defun p -1)
              (unless (form-offset p 1) (return)))
    (move-point point p)))

;;;; Commands

(define-command clojure-indent-sexp () ()
  "Indent the sexp at point."
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-points (current-point) end))))

(define-keys *clojure-mode-keymap*
  ("C-M-q" 'clojure-indent-sexp)
  ("Return" 'newline-and-indent))

;;;; File Types

(define-file-type ("clj" "cljs" "cljc" "cljx" "edn") clojure-mode)
