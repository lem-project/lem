(defpackage :lem-zig-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*zig-mode-hook*
           :zig-mode
           :zig-format-buffer))
(in-package :lem-zig-mode)

;; Variables
(defvar *zig-format-command* "zig fmt"
  "Command used to format Zig source files.")

(defun tokens (boundary strings)
  "Create a regex pattern for matching a list of tokens."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-zig ()
  "Create TextMate language grammar for Zig syntax highlighting."
  (let* ((patterns (make-tm-patterns
                    ;; Line comment
                    (make-tm-region "//" "$" :name 'syntax-comment-attribute)
                    ;; Doc comment
                    (make-tm-region "///" "$" :name 'syntax-comment-attribute)
                    ;; String literals
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")))
                    ;; Multiline string literals
                    (make-tm-region '(:sequence "\\\\\\\\")
                                    "$"
                                    :name 'syntax-string-attribute)
                    ;; Character literals
                    (make-tm-match "'\\\\?.'"
                                   :name 'syntax-string-attribute)
                    ;; Keywords
                    (make-tm-match (tokens :word-boundary
                                           '("addrspace" "align" "allowzero"
                                             "and" "anyframe" "anytype"
                                             "asm" "async" "await"
                                             "break"
                                             "callconv" "catch" "comptime" "const" "continue"
                                             "defer"
                                             "else" "enum" "errdefer" "error" "export" "extern"
                                             "fn" "for"
                                             "if" "inline"
                                             "linksection"
                                             "noalias" "noinline" "nosuspend"
                                             "opaque" "or" "orelse"
                                             "packed" "pub"
                                             "resume" "return"
                                             "struct" "suspend" "switch"
                                             "test" "threadlocal" "try"
                                             "union" "unreachable" "usingnamespace"
                                             "var" "volatile"
                                             "while"))
                                   :name 'syntax-keyword-attribute)
                    ;; Builtin functions (@xxx)
                    (make-tm-match "@[a-zA-Z_][a-zA-Z0-9_]*"
                                   :name 'syntax-builtin-attribute)
                    ;; Function definitions
                    (make-tm-match `(:sequence
                                    (:register "fn")
                                    (:greedy-repetition 1 nil :whitespace-char-class)
                                    ,(ppcre:parse-string "([a-zA-Z_][a-zA-Z0-9_]*)")
                                    :word-boundary)
                                   :captures (vector nil
                                                     (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-function-name-attribute)))
                    ;; Test declarations
                    (make-tm-match `(:sequence
                                    (:register "test")
                                    (:greedy-repetition 1 nil :whitespace-char-class)
                                    ,(ppcre:parse-string "\"([^\"]+)\""))
                                   :captures (vector nil
                                                     (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-string-attribute)))
                    ;; Primitive types
                    (make-tm-match
                     (tokens :word-boundary
                             '("i8" "i16" "i32" "i64" "i128" "isize"
                               "u8" "u16" "u32" "u64" "u128" "usize"
                               "f16" "f32" "f64" "f80" "f128"
                               "c_char" "c_short" "c_int" "c_long" "c_longlong"
                               "c_ushort" "c_uint" "c_ulong" "c_ulonglong"
                               "bool" "void" "anyopaque" "type" "noreturn"
                               "anyerror" "comptime_int" "comptime_float"))
                     :name 'syntax-type-attribute)
                    ;; Type names (capitalized identifiers)
                    (make-tm-match "\\b[A-Z][a-zA-Z_0-9]*\\b"
                                   :name 'syntax-type-attribute)
                    ;; Constants
                    (make-tm-match (tokens :word-boundary
                                           '("null" "undefined" "true" "false"))
                                   :name 'syntax-constant-attribute)
                    ;; Special identifiers
                    (make-tm-match (tokens :word-boundary
                                           '("_"))
                                   :name 'syntax-constant-attribute)
                    ;; Numeric literals (hex, binary, octal, decimal, float)
                    (make-tm-match "\\b0x[0-9a-fA-F_]+\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b0b[01_]+\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b0o[0-7_]+\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b[0-9][0-9_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Operators (for highlighting)
                    (make-tm-match (tokens nil '("+" "+=" "-" "-=" "*" "*=" "/" "/="
                                                 "%" "%=" "=" "==" "!=" "<" "<=" ">"
                                                 ">=" "<<" "<<=" ">>" ">>=" "&" "&="
                                                 "|" "|=" "^" "^=" "~" "!" "?" "."
                                                 ".." "..." "++" "**" "=>" "->" "||"
                                                 ".*" ".?"))
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *zig-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\@)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :expr-prefix-chars '(#\- #\+ #\* #\& #\!)
                :expr-suffix-chars '(#\: #\, #\;)
                :line-comment-string "//")))
    (set-syntax-parser table (make-tmlanguage-zig))
    table)
  "Syntax table for Zig mode.")

;; Mode definition
(define-major-mode zig-mode language-mode
    (:name "Zig"
     :keymap *zig-mode-keymap*
     :syntax-table *zig-syntax-table*
     :mode-hook *zig-mode-hook*
     :formatter 'zig-format-buffer)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'zig-calc-indent
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'beginning-of-defun-function) 'zig-beginning-of-defun
        (variable-value 'end-of-defun-function) 'zig-end-of-defun
        (variable-value 'line-comment) "//"
        (variable-value 'insertion-line-comment) "// "
        (variable-value 'tab-width :buffer) 4))

;; Key bindings
(define-key *zig-mode-keymap* "C-c C-f" 'zig-format-buffer)
(define-key *zig-mode-keymap* "C-c C-r" 'zig-run)
(define-key *zig-mode-keymap* "C-c C-b" 'zig-build)
(define-key *zig-mode-keymap* "C-c C-t" 'zig-test)
(define-key *zig-mode-keymap* "}" 'zig-electric-close)
(define-key *zig-mode-keymap* ")" 'zig-electric-close)
(define-key *zig-mode-keymap* "]" 'zig-electric-close)

;; Functions
(defun zig-beginning-of-defun (point n)
  "Move to the beginning of the current function definition."
  (loop :repeat n :do (search-backward-regexp point "^\\s*\\(pub\\s+\\)?fn\\s+")))

(defun zig-end-of-defun (point n)
  "Move to the end of the current function definition."
  (if (minusp n)
      (zig-beginning-of-defun point (- n))
      (search-forward-regexp point "^}")))

(defun zig-calc-indent (point)
  "Calculate the indentation for the current line in Zig mode."
  (let ((tab-width (variable-value 'tab-width :default point)))
    (with-point ((p point))
      (back-to-indentation p)
      (cond
        ;; Inside string
        ((in-string-p p)
         nil)
        ;; Inside comment
        ((in-comment-p p)
         (with-point ((start p))
           (maybe-beginning-of-comment start)
           (if (eql #\* (character-at (back-to-indentation point)))
               (1+ (point-column start))
               (+ 2 (point-column start)))))
        ;; Closing brace/bracket/paren - match opening
        ((member (character-at p) '(#\} #\] #\)))
         (with-point ((tmp p))
           (when (scan-lists tmp -1 0 t)
             (back-to-indentation tmp)
             (point-column tmp))))
        (t
         ;; General case - look at previous lines
         (let ((indent 0))
           (with-point ((tmp p))
             (when (scan-lists tmp -1 1 t)
               (back-to-indentation tmp)
               (setf indent (+ tab-width (point-column tmp)))))
           ;; Adjust for case/else in switch statements
           (with-point ((tmp p))
             (back-to-indentation tmp)
             (when (or (looking-at tmp "else\\b")
                       (looking-at tmp "=>"))
               (when (> indent 0)
                 (decf indent tab-width))))
           indent))))))

;; Commands
(define-command zig-electric-close (n) (:universal)
  "Insert closing character and re-indent the current line."
  (self-insert n)
  (indent))

(define-command zig-format-buffer (buffer) ((current-buffer))
  "Format the current buffer using zig fmt.
Saves the buffer if modified before formatting."
  (let ((filename (buffer-filename buffer)))
    (when filename
      (when (buffer-modified-p buffer)
        (save-buffer buffer))
      (let ((output (with-output-to-string (out)
                      (uiop:run-program (format nil "~A ~A" *zig-format-command* filename)
                                        :output out
                                        :error-output out
                                        :ignore-error-status t))))
        (revert-buffer t)
        (if (string= output "")
            (message "Formatted buffer with zig fmt.")
            (message "~A" output))))))

(define-command zig-run () ()
  "Run the current Zig file using zig run.
Saves the buffer if modified before running."
  (let ((filename (buffer-filename (current-buffer))))
    (when filename
      (when (buffer-modified-p (current-buffer))
        (save-buffer (current-buffer)))
      (lem-process:run-process (list "zig" "run" filename)
                               :name "*zig-run*"
                               :directory (buffer-directory (current-buffer))))))

(define-command zig-build () ()
  "Build the current Zig project using zig build.
Requires a build.zig file in the project directory."
  (let ((directory (buffer-directory (current-buffer))))
    (lem-process:run-process (list "zig" "build")
                             :name "*zig-build*"
                             :directory directory)))

(define-command zig-test () ()
  "Run tests for the current Zig file using zig test.
Saves the buffer if modified before running tests."
  (let ((filename (buffer-filename (current-buffer))))
    (when filename
      (when (buffer-modified-p (current-buffer))
        (save-buffer (current-buffer)))
      (lem-process:run-process (list "zig" "test" filename)
                               :name "*zig-test*"
                               :directory (buffer-directory (current-buffer))))))

;; File type association
(define-file-type ("zig" "zon") zig-mode)
