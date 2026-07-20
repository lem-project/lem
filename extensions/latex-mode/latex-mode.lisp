(defpackage :lem-latex-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*latex-mode-hook*
           :latex-mode))
(in-package :lem-latex-mode)

(defparameter *latex-verbatim-environments*
  '("verbatim" "Verbatim" "lstlisting" "listing" "minted"))

(defparameter *latex-verbatim-begin-regex*
  "\\\\begin\\s*\\{(?:verbatim|Verbatim|lstlisting|listing|minted)\\}(?:\\[[^\\]]*\\])?(?:\\{[^}]*\\})?")

(defparameter *latex-verbatim-end-regex*
  "\\\\end\\s*\\{(?:verbatim|Verbatim|lstlisting|listing|minted)\\}")

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tm-patterns-latex-math ()
  (make-tm-patterns
   ;; Control sequences and escaped one-char commands inside math.
   (make-tm-match "\\\\[@A-Za-z]+\\*?" :name 'syntax-keyword-attribute)
   (make-tm-match "\\\\." :name 'syntax-keyword-attribute)
   ;; Numbers and operators in formulas.
   (make-tm-match "\\b[0-9]+(\\.[0-9]+)?\\b" :name 'syntax-constant-attribute)
   (make-tm-match "[+\\-*/=<>^_]" :name 'syntax-builtin-attribute)
   (make-tm-match "[\\{\\}\\[\\]\\(\\)]" :name 'syntax-type-attribute)))

(defun make-tmlanguage-latex ()
  (let* ((math-patterns (make-tm-patterns-latex-math))
         (patterns
          (make-tm-patterns
           ;; Verbatim-like blocks: keep inner content unparsed.
           (make-tm-region *latex-verbatim-begin-regex*
                           *latex-verbatim-end-regex*
                           :name 'syntax-string-attribute)
           ;; Line comment
           (make-tm-region '(:sequence "%") "$" :name 'syntax-comment-attribute)
           ;; Control sequence: \command or \@command
           (make-tm-match "\\\\[@A-Za-z]+\\*?" :name 'syntax-keyword-attribute)
           ;; Escaped one-char command: \{ \} \$ \% ...
           (make-tm-match "\\\\." :name 'syntax-keyword-attribute)
           ;; Environment name within \begin{...}/\end{...}
           (make-tm-match "\\\\(begin|end)\\{[^}]+\\}"
                          :name 'syntax-function-name-attribute)
           ;; Inline/Display math with nested highlighting.
           (make-tm-region "\\$\\$" "\\$\\$"
                           :name 'syntax-constant-attribute
                           :patterns math-patterns)
           (make-tm-region "\\$" "\\$"
                           :name 'syntax-constant-attribute
                           :patterns math-patterns)
           (make-tm-region "\\\\\\[" "\\\\\\]"
                           :name 'syntax-constant-attribute
                           :patterns math-patterns)
           (make-tm-region "\\\\\\(" "\\\\\\)"
                           :name 'syntax-constant-attribute
                           :patterns math-patterns)
           ;; Braces/optional args markers
           (make-tm-match "[\\{\\}\\[\\]]" :name 'syntax-type-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *latex-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :line-comment-string "%"
                :string-quote-chars '(#\"))))
    (set-syntax-parser table (make-tmlanguage-latex))
    table))

(defun latex-escaped-char-p (line index)
  (loop :with backslashes := 0
        :for i :downfrom (1- index) :to 0
        :while (char= (char line i) #\\)
        :do (incf backslashes)
        :finally (return (oddp backslashes))))

(defun latex-strip-comment (line)
  (loop :for i :from 0 :below (length line)
        :for ch := (char line i)
        :when (and (char= ch #\%)
                   (not (latex-escaped-char-p line i)))
          :do (return (subseq line 0 i))
        :finally (return line)))

(defun latex-line-blank-p (line)
  (uiop:emptyp (string-trim '(#\space #\tab) line)))

(defun latex-line-comment-only-p (line)
  (and (not (latex-line-blank-p line))
       (char= #\% (char (string-left-trim '(#\space #\tab) line) 0))))

(defun latex-count-matches (regex line)
  (length (ppcre:all-matches-as-strings regex line)))

(defun latex-verbatim-begin-line-p (line)
  (ppcre:scan (format nil "^\\s*~a" *latex-verbatim-begin-regex*)
              (latex-strip-comment line)))

(defun latex-verbatim-end-line-p (line)
  (ppcre:scan (format nil "^\\s*~a" *latex-verbatim-end-regex*)
              (latex-strip-comment line)))

(defun latex-in-verbatim-environment-p (point)
  (with-point ((p point))
    (let ((depth 0))
      (loop
        (unless (line-offset p -1)
          (return nil))
        (let* ((line (latex-strip-comment (line-string p)))
               (end-count (latex-count-matches *latex-verbatim-end-regex* line))
               (begin-count (latex-count-matches *latex-verbatim-begin-regex* line)))
          (incf depth end-count)
          (decf depth begin-count)
          (when (minusp depth)
            (return t)))))))

(defun latex-find-verbatim-begin-indent (point)
  (with-point ((p point))
    (let ((depth 1))
      (loop
        (unless (line-offset p -1)
          (return 0))
        (let* ((line (latex-strip-comment (line-string p)))
               (end-count (latex-count-matches *latex-verbatim-end-regex* line))
               (begin-count (latex-count-matches *latex-verbatim-begin-regex* line)))
          (incf depth end-count)
          (decf depth begin-count)
          (when (<= depth 0)
            (return (point-column (back-to-indentation p)))))))))

(defun latex-closing-line-p (line)
  (let ((code (string-left-trim '(#\space #\tab) (latex-strip-comment line))))
    (or (ppcre:scan "^\\\\end\\s*\\{" code)
        (ppcre:scan "^\\\\\\]" code)
        (ppcre:scan "^\\\\\\)" code))))

(defun latex-opening-line-p (line)
  (let* ((code (latex-strip-comment line))
         (begin-count (latex-count-matches "\\\\begin\\s*\\{" code))
         (end-count (latex-count-matches "\\\\end\\s*\\{" code))
         (display-open-count (latex-count-unescaped-sequence code "\\["))
         (display-close-count (latex-count-unescaped-sequence code "\\]"))
         (inline-open-count (latex-count-unescaped-sequence code "\\("))
         (inline-close-count (latex-count-unescaped-sequence code "\\)")))
    (or (> (- begin-count end-count) 0)
        (> (- display-open-count display-close-count) 0)
        (> (- inline-open-count inline-close-count) 0))))

(defun latex-count-unescaped-sequence (line sequence)
  (loop :with start := 0
        :with count := 0
        :for pos := (search sequence line :start2 start)
        :while pos
        :do (unless (latex-escaped-char-p line pos)
              (incf count))
            (setf start (+ pos (length sequence)))
        :finally (return count)))

(defun latex-brace-balance (line)
  (let ((balance 0)
        (code (latex-strip-comment line)))
    (loop :for i :from 0 :below (length code)
          :for ch := (char code i)
          :unless (latex-escaped-char-p code i)
            :do (cond ((char= ch #\{) (incf balance))
                      ((char= ch #\}) (decf balance))))
    balance))

(defun latex-find-previous-significant-line (point)
  (with-point ((p point))
    (loop
      (unless (line-offset p -1)
        (return nil))
      (let ((line (line-string p)))
        (unless (or (latex-line-blank-p line)
                    (latex-line-comment-only-p line))
          (return (copy-point p :temporary)))))))

(defun latex-next-indent-from-line (line base-indent tab-width)
  (let ((indent base-indent))
    (when (latex-opening-line-p line)
      (incf indent tab-width))
    (let ((balance (latex-brace-balance line)))
      (when (> balance 0)
        (incf indent (* balance tab-width)))
      (when (< balance 0)
        (decf indent (* (abs balance) tab-width))))
    (max 0 indent)))

(defun latex-calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (with-point ((p point))
      (back-to-indentation p)
      (cond
        ((or (in-string-p p) (in-comment-p p))
         nil)
        ((latex-in-verbatim-environment-p p)
         (if (latex-verbatim-end-line-p (line-string p))
             (latex-find-verbatim-begin-indent p)
             (point-column p)))
        (t
         (let ((indent 0)
               (prev (latex-find-previous-significant-line p)))
           (when prev
             (setf indent (point-column (back-to-indentation prev)))
             (setf indent
                   (latex-next-indent-from-line
                    (line-string prev) indent tab-width)))
           (when (latex-closing-line-p (line-string p))
             (decf indent tab-width))
           (max 0 indent)))))))

(define-major-mode latex-mode language-mode
    (:name "LaTeX"
     :keymap *latex-mode-keymap*
     :syntax-table *latex-syntax-table*
     :mode-hook *latex-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'latex-calc-indent
        (variable-value 'line-comment) "%"))

(define-file-type ("tex" "ltx" "sty" "cls") latex-mode)
