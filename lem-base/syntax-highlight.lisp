(in-package :lem-base)

(export '(*syntax-scan-region-function*
          syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute
          *global-syntax-highlight*
          enable-syntax-highlight
          enable-syntax-highlight-p
          make-regex-matcher
          make-syntax-match
          make-syntax-region
          add-syntax-pattern
          make-syntax-patterns
          make-syntax-name
          syntax-scan-range))

(defvar *syntax-scan-region-function*)

(define-editor-variable enable-syntax-highlight nil)
(defvar *global-syntax-highlight* t)

(defun make-regex-matcher (regex)
  (ppcre:create-scanner regex))

(defclass syntax ()
  ((attribute
    :initarg :attribute
    :initform 0
    :reader syntax-attribute)))

(defclass syntax-region (syntax)
  ((begin
    :initarg :begin
    :reader syntax-region-begin)
   (end
    :initarg :end
    :reader syntax-region-end)
   (patterns
    :initarg :patterns
    :initform nil
    :reader syntax-region-patterns)))

(defclass syntax-match (syntax)
  ((matcher
    :initarg :matcher
    :initform nil
    :reader syntax-match-matcher)
   (captures
    :initarg :captures
    :initform nil
    :reader syntax-match-captures)
   (test-symbol
    :initform nil
    :reader syntax-match-test-symbol)
   (matched-symbol
    :initform nil
    :reader syntax-match-matched-symbol)
   (move-action
    :initarg :move-action
    :initform nil
    :reader syntax-match-move-action)))

(defun make-syntax-match (matcher &key attribute captures move-action)
  (make-instance 'syntax-match
                 :matcher matcher
                 :attribute attribute
                 :captures captures
                 :move-action move-action))

(defun make-syntax-region (begin-matcher end-matcher &key attribute patterns)
  (make-instance 'syntax-region
                 :begin begin-matcher
                 :end end-matcher
                 :attribute attribute
                 :patterns patterns))

(defun add-syntax-pattern (syntax-table pattern)
  (push pattern (syntax-table-patterns syntax-table)))

(defun make-syntax-patterns (&rest patterns)
  patterns)

(defun make-syntax-name (&key attribute)
  attribute)

(defun enable-syntax-highlight-p (buffer)
  (and *global-syntax-highlight*
       (variable-value 'enable-syntax-highlight :buffer buffer)))

(defvar *syntax-scan-limit*)
(defvar *syntax-symbol-lifetimes* nil)

(defun set-syntax-context (line x)
  (if (line-%syntax-context line)
      (setf (car (line-%syntax-context line)) x)
      (setf (line-%syntax-context line) (cons x nil))))

(defun get-syntax-context (line)
  (car (line-%syntax-context line)))

(defun set-syntax-lifetimes (line lifetimes)
  (if (line-%syntax-context line)
      (setf (cdr (line-%syntax-context line)) lifetimes)
      (setf (line-%syntax-context line) (cons nil lifetimes))))

(defun get-syntax-lifetimes (line)
  (cdr (line-%syntax-context line)))
