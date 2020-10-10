(defpackage :lem-lsp-mode/typescript
  (:use :cl :alexandria)
  (:import-from :cl-change-case)
  (:import-from :trivial-types)
  (:import-from :cl-ppcre))
(in-package :lem-lsp-mode/typescript)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun whitespacep (char)
  (member char '(#\space #\tab #\newline)))

(defclass token ()
  ((string :initarg :string :reader token-string)))

(defclass word (token) ())
(defclass comment (token) ())
(defclass line-comment (comment) ())
(defclass block-comment (comment) ((start-column :initarg :start-column)))
(defclass string-literal (token) ())
(defclass number-literal (token) ())
(defclass operator (token) ())

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (token-string object) stream)))

(defclass iterator ()
  ((list :initarg :list)))

(defun next (iterator)
  (pop (slot-value iterator 'list)))

(defun lookahead (iterator)
  (first (slot-value iterator 'list)))

(defun scan-ahead (text &optional (start 0))
  (multiple-value-bind (start end start-groups end-groups)
      (ppcre:scan "^\\s*([a-zA-Z0-9_]+|[-+]?[0-9]+(:?\\.[0-9]+)|\\?|//|/\\*|.)" text :start start)
    (when start
      (let ((str (subseq text (aref start-groups 0) (aref end-groups 0))))
        (values str end)))))

(defun word-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun get-column (text pos)
  (if (char= #\newline (char text pos))
      0
      (loop :for i := pos :then (1- i)
            :do (cond ((< i 0)
                       (return pos))
                      ((char= #\newline (char text i))
                       (return (- pos i 1)))))))

(defun tokenize (text)
  (let ((pos 0)
        (str)
        (tokens '()))
    (loop
      (setf (values str pos) (scan-ahead text pos))
      (unless str (return))
      (cond ((string= str "/*")
             (let ((end (search "*/" text :start2 (+ pos 2))))
               (assert end)
               (push (make-instance 'block-comment :string (subseq text pos end) :start-column (get-column text pos))
                     tokens)
               (setf pos (+ end 2))))
            ((string= str "//")
             (let ((end (position #\newline text :start pos)))
               (assert end)
               (push (make-instance 'line-comment :string (subseq text pos end))
                     tokens)
               (setf pos (1+ end))))
            ((string= str "'")
             (let ((string-start pos))
               (let ((end (search "'" text :start2 (1+ pos))))
                 (assert end)
                 (push (make-instance 'string-literal :string (subseq text string-start end))
                       tokens)
                 (setf pos (1+ end)))))
            ((digit-char-p (char str 0))
             (push (make-instance 'number-literal :string str) tokens))
            ((word-char-p (char str 0))
             (push (make-instance 'word :string str) tokens))
            (t
             (push (make-instance 'operator :string str) tokens))))
    (make-instance 'iterator :list (nreverse tokens))))

(defclass interface ()
  ((elements :initarg :elements)))

(defclass named-interface (interface)
  ((name :initarg :name)
   (extends :initarg :extends)))

(defclass element ()
  ((name :initarg :name)
   (optional-p :initarg :optional-p)
   (comment :initarg :comment)
   (type-expression :initarg :type-expression)))

(defclass type-or ()
  ((types :initarg :types)))

(defclass simple-type ()
  ((name :initarg :name)))

(defclass array-type ()
  ((name :initarg :name)))

(defclass string-expression ()
  ((string :initarg :string)))

(defvar *token-iterator*)

(defun next-token ()
  (next *token-iterator*))

(defun ahead-token ()
  (lookahead *token-iterator*))

(defun match (token-type &optional string)
  (let ((token (ahead-token)))
    (when (and (typep token token-type)
               (or (null string)
                   (string= (token-string token) string)))
      token)))

(defun accept (token-type &optional string)
  (when (match token-type string)
    (next-token)))

(defun exact (token-type &optional string)
  (or (accept token-type string)
      (error "Token not expected: ~S" (ahead-token))))

(defun parse-type-expression ()
  (labels ((type-name ()
             (when-let ((name (accept 'word)))
               (cond ((accept 'operator "[")
                      (exact 'operator "]")
                      (make-instance 'array-type
                                     :name (token-string name)))
                     (t
                      (make-instance 'simple-type
                                     :name (token-string name))))))
           (string-literal ()
             (when-let ((token (accept 'string-literal)))
               (make-instance 'string-expression
                              :string (token-string token))))
           (type-1 ()
             (cond ((match 'operator "{")
                    (parse-interface-expression))
                   ((type-name))
                   ((string-literal))))
           (type-or ()
             (let ((types
                     (loop :for type := (type-1)
                           :collect type
                           :while (accept 'operator "|"))))
               (if (length= types 1)
                   (first types)
                   (make-instance 'type-or :types types)))))
    (type-or)))

(defun comment-to-string (comment)
  (ppcre:regex-replace-all "\\n\\s*" (token-string comment) (string #\newline)))

(defun parse-interface-expression ()
  (exact 'operator "{")
  (let ((elements '()))
    (loop
      (let ((comment
              (string-trim '(#\newline #\space #\tab)
                           (with-output-to-string (out)
                             (loop :for comment := (accept 'comment)
                                   :while comment
                                   :do (write-line (comment-to-string comment) out))))))
        (if-let ((element-name (accept 'word)))
          (let ((optional-p (not (null (accept 'operator "?")))))
            (exact 'operator ":")
            (let ((type-expression (parse-type-expression)))
              (accept 'operator ";")
              (push (make-instance 'element
                                   :name (token-string element-name)
                                   :optional-p optional-p
                                   :type-expression type-expression
                                   :comment comment)
                    elements)))
          (return))))
    (exact 'operator "}")
    (make-instance 'interface :elements (nreverse elements))))

(defun parse-def-interface ()
  (accept 'word "export")
  (when (accept 'word "interface")
    (let ((interface-name (token-string (exact 'word)))
          (extends-interface
            (when (accept 'word "extends")
              (token-string (accept 'word)))))
      (let ((interface (parse-interface-expression)))
        (change-class interface
                      'named-interface
                      :name interface-name
                      :extends extends-interface)))))

(defun parse (*token-iterator*)
  (loop :while (accept 'comment))
  (parse-def-interface))

(defun symbolize (string &optional package)
  (let ((name (string-upcase (cl-change-case:param-case string))))
    (if package
        (intern name package)
        (intern name))))

(defun element-to-slot-specifier (element)
  (with-slots (name optional-p comment type-expression) element
    `(,(if optional-p
           (symbolicate (symbolize name) '?)
           (symbolize name))
      :initarg ,(symbolize name :keyword)
      :documentation ,comment)))

(defun to-lisp (named-interface)
  (with-slots (name extends elements) named-interface
    `(defclass ,(symbolize name) ,(if extends `(,(symbolize extends)) ())
       ,(mapcar #'element-to-slot-specifier elements))))
