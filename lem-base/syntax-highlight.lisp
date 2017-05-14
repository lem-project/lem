(in-package :lem-base)

(export '(syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute
          *global-syntax-highlight*
          enable-syntax-highlight
          enable-syntax-highlight-p
          make-tmlanguage
          make-regex-matcher
          make-syntax-match
          make-tm-region
          add-syntax-pattern
          make-syntax-patterns
          make-syntax-name
          syntax-scan-region))

(define-editor-variable enable-syntax-highlight nil)
(defvar *global-syntax-highlight* t)

(defun enable-syntax-highlight-p (buffer)
  (and *global-syntax-highlight*
       (variable-value 'enable-syntax-highlight :buffer buffer)))

(defun current-syntax-parser ()
  (syntax-table-parser (current-syntax)))

(defclass syntax-parser ()
  ())

(defgeneric %syntax-scan-region (parser start end))

(defun syntax-scan-region (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (let ((buffer (point-buffer start)))
    (when (enable-syntax-highlight-p buffer)
      (let ((*current-syntax*
             (buffer-syntax-table buffer)))
        (with-point ((start start)
                     (end end))
          (line-start start)
          (line-end end)
          (%syntax-scan-region (syntax-table-parser *current-syntax*) start end))))))


(defclass tmlanguage (syntax-parser)
  ((patterns
    :initarg :patterns
    :initform nil
    :accessor tmlanguage-patterns)))

(defclass syntax ()
  ((attribute
    :initarg :attribute
    :initform 0
    :reader syntax-attribute)))

(defclass tm-region (syntax)
  ((begin
    :initarg :begin
    :reader tm-region-begin)
   (end
    :initarg :end
    :reader tm-region-end)
   (patterns
    :initarg :patterns
    :initform nil
    :reader tm-region-patterns)))

(defclass syntax-match (syntax)
  ((matcher
    :initarg :matcher
    :initform nil
    :reader syntax-match-matcher)
   (captures
    :initarg :captures
    :initform nil
    :reader syntax-match-captures)
   (move-action
    :initarg :move-action
    :initform nil
    :reader syntax-match-move-action)))

(defun make-tmlanguage ()
  (make-instance 'tmlanguage))

(defun make-syntax-match (matcher &key attribute captures move-action)
  (make-instance 'syntax-match
                 :matcher matcher
                 :attribute attribute
                 :captures captures
                 :move-action move-action))

(defun make-tm-region (begin-matcher end-matcher &key attribute patterns)
  (make-instance 'tm-region
                 :begin begin-matcher
                 :end end-matcher
                 :attribute attribute
                 :patterns patterns))

(defun make-regex-matcher (regex)
  (ppcre:create-scanner regex))

(defun make-syntax-patterns (&rest patterns)
  patterns)

(defun make-syntax-name (&key attribute)
  attribute)

(defun add-syntax-pattern (tmlanguage pattern)
  (push pattern (tmlanguage-patterns tmlanguage)))

(defmethod %syntax-scan-region ((tmlanguage tmlanguage) start end)
  (tm-syntax-scan-region start end))
