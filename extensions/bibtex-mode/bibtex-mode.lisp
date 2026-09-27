(defpackage :lem-bibtex-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*bibtex-mode-hook*
           :bibtex-mode))
(in-package :lem-bibtex-mode)

(defun make-tmlanguage-bibtex ()
  (let ((patterns
          (make-tm-patterns
           ;; Line comment
           (make-tm-region '(:sequence "%") "$" :name 'syntax-comment-attribute)
           ;; String values
           (make-tm-region '(:sequence "\"")
                           '(:sequence "\"")
                           :name 'syntax-string-attribute
                           :patterns (make-tm-patterns
                                      (make-tm-match "\\\\.")))
           ;; Entry declarations: @article, @book, @string...
           (make-tm-match "@[A-Za-z]+" :name 'syntax-keyword-attribute)
           ;; Field names
           (make-tm-match "^\\s*[A-Za-z][A-Za-z0-9_-]*\\s*="
                          :name 'syntax-variable-attribute)
           ;; Numbers
           (make-tm-match "\\b[0-9]+\\b" :name 'syntax-constant-attribute)
           ;; Delimiters and assignment
           (make-tm-match "[{}(),=]" :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *bibtex-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\{ . #\})
                               (#\( . #\)))
                :line-comment-string "%"
                :string-quote-chars '(#\"))))
    (set-syntax-parser table (make-tmlanguage-bibtex))
    table))

(defun bibtex-line-blank-p (line)
  (uiop:emptyp (string-trim '(#\space #\tab) line)))

(defun bibtex-line-comment-only-p (line)
  (let ((trimmed (string-left-trim '(#\space #\tab) line)))
    (and (not (uiop:emptyp trimmed))
         (char= #\% (char trimmed 0)))))

(defun bibtex-escaped-char-p (line index)
  (loop :with backslashes := 0
        :for i :downfrom (1- index) :to 0
        :while (char= (char line i) #\\)
        :do (incf backslashes)
        :finally (return (oddp backslashes))))

(defun bibtex-brace-balance (line)
  (let ((balance 0)
        (in-string nil))
    (loop :for i :from 0 :below (length line)
          :for ch := (char line i)
          :do (cond
                ((and (char= ch #\")
                      (or (zerop i)
                          (not (char= (char line (1- i)) #\\))))
                 (setf in-string (not in-string)))
                ((and (not in-string)
                      (char= ch #\%)
                      (not (bibtex-escaped-char-p line i)))
                 (return))
                ((not in-string)
                 (cond ((char= ch #\{) (incf balance))
                       ((char= ch #\}) (decf balance))
                       ((char= ch #\() (incf balance))
                       ((char= ch #\)) (decf balance))))))
    balance))

(defun bibtex-find-previous-significant-line (point)
  (with-point ((p point))
    (loop
      (unless (line-offset p -1)
        (return nil))
      (let ((line (line-string p)))
        (unless (or (bibtex-line-blank-p line)
                    (bibtex-line-comment-only-p line))
          (return (copy-point p :temporary)))))))

(defun bibtex-closing-line-p (line)
  (ppcre:scan "^\\s*[})]" line))

(defun bibtex-leading-close-depth (line)
  (or (loop :for ch :across (string-left-trim '(#\space #\tab) line)
            :while (or (char= ch #\}) (char= ch #\)))
            :count t)
      0))

(defun bibtex-calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (with-point ((p point))
      (back-to-indentation p)
      (cond
        ((or (in-string-p p) (in-comment-p p))
         nil)
        (t
         (let ((indent 0)
               (prev (bibtex-find-previous-significant-line p)))
           (when prev
             (setf indent (point-column (back-to-indentation prev)))
             (let ((balance (bibtex-brace-balance (line-string prev))))
               (incf indent (* balance tab-width))))
           (when (bibtex-closing-line-p (line-string p))
             (decf indent
                   (* tab-width
                      (bibtex-leading-close-depth (line-string p)))))
           (max 0 indent)))))))

(define-major-mode bibtex-mode language-mode
    (:name "BibTeX"
     :keymap *bibtex-mode-keymap*
     :syntax-table *bibtex-syntax-table*
     :mode-hook *bibtex-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'bibtex-calc-indent
        (variable-value 'line-comment) "%"))

(define-file-type ("bib") bibtex-mode)
