(defpackage :lem-perl-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*perl-mode-hook*
           :perl-mode))
(in-package :lem-perl-mode)

(defun tokens (boundary strings)
  "Create a regex pattern for matching a list of tokens."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-perl ()
  "Create TextMate language grammar for Perl syntax highlighting."
  (let* ((patterns (make-tm-patterns
                    ;; POD documentation
                    (make-tm-region "^=\\w+" "^=cut" :name 'syntax-comment-attribute)
                    ;; Line comment
                    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
                    ;; Double-quoted strings with interpolation
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")))
                    ;; Single-quoted strings (no interpolation)
                    (make-tm-region '(:sequence "'")
                                    '(:sequence "'")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")))
                    ;; Backtick strings (command execution)
                    (make-tm-region '(:sequence "`")
                                    '(:sequence "`")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")))
                    ;; Here-doc strings
                    (make-tm-region "<<['\"]?\\w+['\"]?" "^\\w+$"
                                    :name 'syntax-string-attribute)
                    ;; Regex patterns
                    (make-tm-match "\\b(?:qr|m|s|tr|y)/[^/]*/[^/]*/"
                                   :name 'syntax-string-attribute)
                    ;; Keywords - control flow
                    (make-tm-match (tokens :word-boundary
                                           '("if" "elsif" "else" "unless"
                                             "while" "until" "for" "foreach"
                                             "do" "next" "last" "redo"
                                             "return" "goto" "continue"
                                             "given" "when" "default"
                                             "try" "catch" "finally"))
                                   :name 'syntax-keyword-attribute)
                    ;; Keywords - declarations
                    (make-tm-match (tokens :word-boundary
                                           '("sub" "my" "our" "local" "state"
                                             "package" "use" "require" "no"
                                             "BEGIN" "END" "CHECK" "INIT" "UNITCHECK"
                                             "AUTOLOAD" "DESTROY"
                                             "class" "method" "field" "has"))
                                   :name 'syntax-keyword-attribute)
                    ;; Keywords - other
                    (make-tm-match (tokens :word-boundary
                                           '("and" "or" "not" "xor"
                                             "eq" "ne" "lt" "gt" "le" "ge" "cmp"
                                             "undef" "defined" "bless" "ref"
                                             "wantarray" "caller" "eval" "die" "warn"
                                             "exit" "exec" "system" "fork"))
                                   :name 'syntax-keyword-attribute)
                    ;; Subroutine definitions
                    (make-tm-match `(:sequence
                                    (:register "sub")
                                    (:greedy-repetition 1 nil :whitespace-char-class)
                                    ,(ppcre:parse-string "([a-zA-Z_][a-zA-Z0-9_]*)")
                                    :word-boundary)
                                   :captures (vector nil
                                                     (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-function-name-attribute)))
                    ;; Package declarations
                    (make-tm-match `(:sequence
                                    (:register "package")
                                    (:greedy-repetition 1 nil :whitespace-char-class)
                                    ,(ppcre:parse-string "([a-zA-Z_][a-zA-Z0-9_:]*)")
                                    :word-boundary)
                                   :captures (vector nil
                                                     (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-type-attribute)))
                    ;; Built-in functions
                    (make-tm-match (tokens :word-boundary
                                           '("print" "printf" "say" "open" "close" "read"
                                             "write" "readline" "getc" "eof" "tell" "seek"
                                             "rewind" "stat" "lstat" "truncate"
                                             "push" "pop" "shift" "unshift" "splice"
                                             "reverse" "sort" "map" "grep" "join" "split"
                                             "chomp" "chop" "substr" "index" "rindex"
                                             "sprintf" "length" "uc" "lc" "ucfirst" "lcfirst"
                                             "pack" "unpack" "vec"
                                             "keys" "values" "each" "exists" "delete"
                                             "scalar" "wantarray" "defined"
                                             "ref" "bless" "tie" "untie" "tied"
                                             "localtime" "gmtime" "time" "sleep"
                                             "chdir" "mkdir" "rmdir" "opendir" "closedir"
                                             "readdir" "rewinddir" "telldir" "seekdir"
                                             "link" "unlink" "rename" "symlink" "readlink"
                                             "chmod" "chown" "chroot"
                                             "hex" "oct" "int" "abs" "sqrt" "rand" "srand"
                                             "sin" "cos" "atan2" "exp" "log"
                                             "quotemeta" "lc" "uc" "lcfirst" "ucfirst"
                                             "qw" "qq" "q"))
                                   :name 'syntax-builtin-attribute)
                    ;; Special variables
                    (make-tm-match "\\$[_a-zA-Z][a-zA-Z0-9_]*"
                                   :name 'syntax-variable-attribute)
                    (make-tm-match "@[_a-zA-Z][a-zA-Z0-9_]*"
                                   :name 'syntax-variable-attribute)
                    (make-tm-match "%[_a-zA-Z][a-zA-Z0-9_]*"
                                   :name 'syntax-variable-attribute)
                    ;; Special Perl variables
                    (make-tm-match "\\$[0-9]+"
                                   :name 'syntax-variable-attribute)
                    (make-tm-match "\\$[\\$&`'+*./|,\\\\;#%=\\-~^:?!@]"
                                   :name 'syntax-variable-attribute)
                    (make-tm-match "@[_+\\-]"
                                   :name 'syntax-variable-attribute)
                    (make-tm-match "%ENV|%SIG|%INC"
                                   :name 'syntax-variable-attribute)
                    ;; Constants
                    (make-tm-match (tokens :word-boundary
                                           '("__FILE__" "__LINE__" "__PACKAGE__"
                                             "__SUB__" "__DATA__" "__END__"))
                                   :name 'syntax-constant-attribute)
                    ;; Numeric literals - hex
                    (make-tm-match "\\b0x[0-9a-fA-F_]+\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Numeric literals - binary
                    (make-tm-match "\\b0b[01_]+\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Numeric literals - octal
                    (make-tm-match "\\b0[0-7_]+\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Numeric literals - decimal and float
                    (make-tm-match "\\b[0-9][0-9_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?\\b"
                                   :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *perl-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\$ #\@ #\% #\&)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :expr-prefix-chars '(#\- #\+ #\* #\& #\! #\~ #\\)
                :expr-suffix-chars '(#\: #\, #\;)
                :line-comment-string "#")))
    (set-syntax-parser table (make-tmlanguage-perl))
    table)
  "Syntax table for Perl mode.")

(define-major-mode perl-mode language-mode
    (:name "Perl"
     :keymap *perl-mode-keymap*
     :syntax-table *perl-syntax-table*
     :mode-hook *perl-mode-hook*)
  "Major mode for editing Perl source files."
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'perl-calc-indent
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'beginning-of-defun-function) 'perl-beginning-of-defun
        (variable-value 'end-of-defun-function) 'perl-end-of-defun
        (variable-value 'line-comment) "#"
        (variable-value 'insertion-line-comment) "# "
        (variable-value 'tab-width :buffer) 4))

;; Key bindings
(define-key *perl-mode-keymap* "}" 'perl-electric-close)
(define-key *perl-mode-keymap* ")" 'perl-electric-close)
(define-key *perl-mode-keymap* "]" 'perl-electric-close)

(defun perl-beginning-of-defun (point n)
  "Move to the beginning of the current subroutine definition."
  (loop :repeat n :do (search-backward-regexp point "^\\s*sub\\s+")))

(defun perl-end-of-defun (point n)
  "Move to the end of the current subroutine definition."
  (if (minusp n)
      (perl-beginning-of-defun point (- n))
      (search-forward-regexp point "^}")))

(defun perl-calc-indent (point)
  "Calculate the indentation for the current line in Perl mode."
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
           (point-column start)))
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
           indent))))))

(define-command perl-electric-close (n) (:universal)
  "Insert closing character and re-indent the current line."
  (self-insert n)
  (indent))

;; File type associations
(define-file-type ("pl" "pm" "t" "pod" "psgi") perl-mode)
