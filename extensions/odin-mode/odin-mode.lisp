(defpackage :lem-odin-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*odin-mode-hook*
           :odin-mode
           :odin-format-buffer))
(in-package :lem-odin-mode)

;;; Variables

(defvar *odin-format-command* '("odinfmt" "-stdin")
  "Command used by `odin-format-buffer'.
It reads from standard input and writes the formatted source to standard output.
Ships with ols.")

(defvar *odin-keywords*
  '("asm" "auto_cast" "bit_field" "bit_set" "break" "case" "cast" "context"
    "continue" "defer" "distinct" "do" "dynamic" "else" "enum" "fallthrough"
    "for" "foreign" "if" "import" "in" "inline" "map" "matrix" "no_inline"
    "not_in" "or_break" "or_continue" "or_else" "or_return" "package" "proc"
    "return" "struct" "switch" "transmute" "typeid" "union" "using" "when"
    "where")
  "Odin keywords, based on core:odin/tokenizer.")

(defvar *odin-types*
  '("any" "b8" "b16" "b32" "b64" "bool" "byte" "complex32" "complex64"
    "complex128" "cstring" "f16" "f16be" "f16le" "f32" "f32be" "f32le" "f64"
    "f64be" "f64le" "i8" "i16" "i16be" "i16le" "i32" "i32be" "i32le" "i64"
    "i64be" "i64le" "i128" "i128be" "i128le" "int" "quaternion64"
    "quaternion128" "quaternion256" "rawptr" "rune" "string" "u8" "u16"
    "u16be" "u16le" "u32" "u32be" "u32le" "u64" "u64be" "u64le" "u128"
    "u128be" "u128le" "uint" "uintptr")
  "Odin built-in types.")

(defvar *odin-builtins*
  '("abs" "align_of" "append" "append_elem" "append_elems" "append_string"
    "assert" "assign_at" "cap" "card" "clamp" "clear" "complex" "conj" "copy"
    "delete" "ensure" "expand_values" "free" "free_all" "imag" "inject_at"
    "jmag" "kmag" "len" "make" "max" "min" "new" "new_clone" "offset_of"
    "ordered_remove" "panic" "pop" "pop_front" "pop_front_safe" "pop_safe"
    "quaternion" "raw_data" "real" "remove_range" "reserve" "resize" "shrink"
    "size_of" "soa_unzip" "soa_zip" "swizzle" "type_info_of" "type_of"
    "typeid_of" "unimplemented" "unordered_remove" "unreachable")
  "Procedures from base:builtin.")

(defvar *odin-constants* '("true" "false" "nil")
  "Odin literal constants.")

(defvar *odin-identifier* "[A-Za-z_][A-Za-z_0-9]*"
  "Regular expression matching a single identifier.")

(defun identifier-register ()
  "Return a ppcre parse tree capturing one identifier."
  `(:register ,(ppcre:parse-string *odin-identifier*)))

(defun tokens (boundary strings)
  "Build a ppcre parse tree."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun odin-skip-block-comment (point)
  "Move point past the block comment. In Odin block comments can nest."
  (character-offset point 2)
  (loop :with depth := 1
        :do (unless (search-forward-regexp point "/\\*|\\*/")
              (return (buffer-end point)))
            (if (eql #\/ (character-at point -1))
                (decf depth)
                (incf depth))
            (when (zerop depth)
              (return point))))

(defun make-tmlanguage-odin ()
  "Create the TextMate grammar."
  (let* ((patterns
           (make-tm-patterns
            ;; Comments.  The block comment is a match with a move action
            ;; rather than a region so that nesting is handled.
            (make-tm-match "/\\*"
                           :name 'syntax-comment-attribute
                           :move-action #'odin-skip-block-comment)
            (make-tm-region "//" "$" :name 'syntax-comment-attribute)
            ;; Raw strings perform no escape processing at all.
            (make-tm-region "`" "`" :name 'syntax-string-attribute)
            (make-tm-region '(:sequence "\"")
                            '(:sequence "\"")
                            :name 'syntax-string-attribute
                            :patterns (make-tm-patterns
                                       (make-tm-match "\\\\.")))
            (make-tm-match "'(?:\\\\.|[^'\\\\])'"
                           :name 'syntax-string-attribute)
            ;; Attributes such as @(private="file") may span several lines,
            ;; so only the opening `@(name' is highlighted.
            (make-tm-match (format nil "@\\(?~A" *odin-identifier*)
                           :name 'syntax-builtin-attribute)
            ;; Directives are expressions, not preprocessor lines: `#partial
            ;; switch' must not swallow the rest of the line.
            (make-tm-match (format nil "#~A" *odin-identifier*)
                           :name 'syntax-builtin-attribute)
            ;; `name :: proc' and `name :: struct' carry the declaration.
            (make-tm-match `(:sequence
                             :word-boundary
                             ,(identifier-register)
                             (:greedy-repetition 0 nil :whitespace-char-class)
                             (:register "::")
                             (:greedy-repetition 0 nil :whitespace-char-class)
                             (:register "proc")
                             :word-boundary)
                           :captures (vector nil
                                             (make-tm-name 'syntax-function-name-attribute)
                                             (make-tm-name 'syntax-keyword-attribute)
                                             (make-tm-name 'syntax-keyword-attribute)))
            (make-tm-match `(:sequence
                             :word-boundary
                             ,(identifier-register)
                             (:greedy-repetition 0 nil :whitespace-char-class)
                             (:register "::")
                             (:greedy-repetition 0 nil :whitespace-char-class)
                             (:register (:alternation "struct" "union" "enum"
                                                      "bit_field" "bit_set"
                                                      "distinct" "matrix" "map"))
                             :word-boundary)
                           :captures (vector nil
                                             (make-tm-name 'syntax-type-attribute)
                                             (make-tm-name 'syntax-keyword-attribute)
                                             (make-tm-name 'syntax-keyword-attribute)))
            ;; Only package-qualified calls are matched; a bare `name(' is
            ;; indistinguishable from `if (' without a parser.
            (make-tm-match `(:sequence
                             :word-boundary
                             ,(identifier-register)
                             "."
                             ,(identifier-register)
                             (:greedy-repetition 0 nil :whitespace-char-class)
                             "(")
                           :captures (vector nil
                                             nil
                                             (make-tm-name 'syntax-function-name-attribute)))
            (make-tm-match (tokens :word-boundary *odin-keywords*)
                           :name 'syntax-keyword-attribute)
            (make-tm-match (tokens :word-boundary *odin-types*)
                           :name 'syntax-type-attribute)
            (make-tm-match (tokens :word-boundary *odin-builtins*)
                           :name 'syntax-builtin-attribute)
            (make-tm-match (tokens :word-boundary *odin-constants*)
                           :name 'syntax-constant-attribute)
            ;; `::' declares a constant and `:=' a variable; both are worth
            ;; distinguishing from the plain `:' of a type annotation.
            (make-tm-match (tokens nil '("::" ":=" "->" "---" "..=" "..<" ".."))
                           :name 'syntax-keyword-attribute)
            (make-tm-match (concatenate 'string
                                        "\\b(?:0[bB][01_]+|0[oO][0-7_]+|0[dD][0-9_]+"
                                        "|0[zZ][0-9abAB_]+|0[xX][0-9a-fA-F_]+"
                                        "|[0-9][0-9_]*(?:\\.[0-9][0-9_]*)?(?:[eE][-+]?[0-9_]+)?)"
                                        "[ijk]?\\b")
                           :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *odin-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :expr-prefix-chars '(#\- #\+ #\& #\^)
                :expr-suffix-chars '(#\: #\, #\;)
                :block-string-pairs '(("`" . "`"))
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (make-tmlanguage-odin)))
    (set-syntax-parser table tmlanguage)
    table))

;;; Major Mode Definition

(define-major-mode odin-mode language-mode
    (:name "Odin"
     :keymap *odin-mode-keymap*
     :syntax-table *odin-syntax-table*
     :mode-hook *odin-mode-hook*
     :formatter 'odin-format)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'odin-calc-indent
        ;; Odin's standard formatting indents with tabs.
        (variable-value 'indent-tabs-mode) t
        (variable-value 'beginning-of-defun-function) 'odin-beginning-of-defun
        (variable-value 'end-of-defun-function) 'odin-end-of-defun
        (variable-value 'line-comment) "//"
        (variable-value 'insertion-line-comment) "// "
        (variable-value 'tab-width :buffer) 4))

(define-key *odin-mode-keymap* "C-c C-f" 'odin-format-buffer)

;;; Navigation

(defun odin-beginning-of-defun (point n)
  "Move POINT backward across N Odin declarations.
Odin declarations are `name :: proc' and friends at column 0, so no return
type has to be skipped the way it does in C."
  (loop :repeat n
        :do (search-backward-regexp
             point
             (format nil "^~A\\s*::\\s*(?:proc|struct|union|enum|bit_field)\\b"
                     *odin-identifier*))))

(defun odin-end-of-defun (point n)
  "Move POINT forward to the end of the current Odin declaration."
  (if (minusp n)
      (odin-beginning-of-defun point (- n))
      (search-forward-regexp point "^\\}")))

;;; Indentation

(defun odin-where-clause-line-p (point)
  "True when the line at POINT begins with a `where' clause."
  (with-point ((p point))
    (back-to-indentation p)
    (and (looking-at p "where\\b") t)))

(defun odin-enclosing-block-column (point)
  "Column the block enclosing POINT is indented from, or NIL at top level.
A `where' clause is itself indented one level below its declaration, yet
the body whose brace it carries belongs to the declaration, so a brace
opened on a `where' line is measured from the line above it."
  (with-point ((tmp point))
    (when (scan-lists tmp -1 1 t)
      (back-to-indentation tmp)
      (when (and (odin-where-clause-line-p tmp)
                 (line-offset tmp -1))
        (back-to-indentation tmp))
      (point-column tmp))))

(defun odin-calc-indent (point)
  "Calculate the indentation of the line at POINT.
Odin is brace delimited, so the enclosing block gives the base indent;
`case' labels sit at the level of their `switch' and `where' clauses one
level in from the declaration they constrain."
  (let ((tab-width (variable-value 'tab-width :default point)))
    (with-point ((p point))
      (back-to-indentation p)
      (cond
        ((in-string-p p)
         nil)
        ((in-comment-p p)
         ;; A `*'-prefixed continuation line aligns under the opening `/*'.
         ;; Odin's doc comments run flush against the margin instead, so
         ;; anything else keeps the indentation it already has.
         (if (eql #\* (character-at p))
             (with-point ((start p))
               (maybe-beginning-of-comment start)
               (1+ (point-column start)))
             (point-column p)))
        ((member (character-at p) '(#\} #\] #\)))
         (or (odin-enclosing-block-column p) 0))
        (t
         (let ((indent (alexandria:if-let ((column (odin-enclosing-block-column p)))
                         (+ column tab-width)
                         0)))
           (cond ((looking-at p "case\\b")
                  (max 0 (- indent tab-width)))
                 ((looking-at p "where\\b")
                  (+ indent tab-width))
                 (t
                  indent))))))))

;;; Formatting

(defun odin-run-formatter (text directory)
  "Pipe TEXT through `*odin-format-command*' run in DIRECTORY.
Returns the formatted text, or NIL and a message describing the failure."
  (handler-case
      (multiple-value-bind (output error-output status)
          (with-input-from-string (input text)
            (uiop:run-program *odin-format-command*
                              :directory directory
                              :input input
                              :output :string
                              :error-output :string
                              :ignore-error-status t))
        (if (zerop status)
            (values output nil)
            (values nil (string-trim '(#\newline #\space) error-output))))
    (error (e)
      (values nil (princ-to-string e)))))

(defun odin-replace-with-formatted (buffer)
  "Replace BUFFER with its odinfmt output, keeping the cursor on its line.
Returns true on success, or NIL and a message describing the failure."
  (let* ((start (buffer-start-point buffer))
         (end (buffer-end-point buffer))
         (text (points-to-string start end)))
    (multiple-value-bind (formatted error-message)
        (odin-run-formatter text (buffer-directory buffer))
      (cond ((null formatted)
             (values nil error-message))
            (t
             (unless (string= formatted text)
               (let* ((point (buffer-point buffer))
                      (line (line-number-at-point point))
                      (charpos (point-charpos point)))
                 (delete-between-points start end)
                 (insert-string (buffer-point buffer) formatted)
                 ;; Formatting can drop lines, and `move-to-line' refuses to
                 ;; move at all past the last one, which would strand the
                 ;; cursor at the end of the buffer.
                 (move-to-line point (min line (buffer-nlines buffer)))
                 (line-offset point 0 charpos)))
             (values t nil))))))

(defun odin-format (buffer)
  "Format BUFFER with odinfmt, reporting failures in the echo area.
This is the formatter registered for `odin-mode': `lem/format:format-buffer'
turns anything signalled by a formatter into a generic \"No formatter for
mode\" notice, so the odinfmt error has to be reported here instead."
  (multiple-value-bind (successp error-message) (odin-replace-with-formatted buffer)
    (if successp
        (message "Formatted buffer with odinfmt.")
        (message "odinfmt failed: ~A" error-message))))

(define-command odin-format-buffer (buffer) ((current-buffer))
  "Format BUFFER with odinfmt, keeping the cursor on its current line."
  (multiple-value-bind (successp error-message) (odin-replace-with-formatted buffer)
    (unless successp
      (editor-error "odinfmt failed: ~A" error-message))
    (message "Formatted buffer with odinfmt.")))

(define-file-type ("odin") odin-mode)
