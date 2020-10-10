(defpackage :lem-lsp-mode/typescript
  (:use :cl :alexandria)
  (:import-from :cl-change-case)
  (:import-from :trivial-types)
  (:import-from :cl-ppcre)
  (:export :translate))
(in-package :lem-lsp-mode/typescript)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(define-condition ts-parse-error ()
  ((message :initarg :message
            :reader ts-parse-error-message)
   (position :initarg :position
             :initform nil
             :reader ts-parse-error-position))
  (:report (lambda (c s)
             (format s
                     "~A: ~A"
                     (ts-parse-error-position c)
                     (ts-parse-error-message c)))))

(defun whitespacep (char)
  (member char '(#\space #\tab #\newline)))

(defclass token ()
  ((string :initarg :string :reader token-string)
   (position :initarg :position :reader token-position)))

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

(defun end-p (iterator)
  (endp (slot-value iterator 'list)))

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
               (push (make-instance 'block-comment
                                    :position pos
                                    :string (subseq text pos end)
                                    :start-column (get-column text pos))
                     tokens)
               (setf pos (+ end 2))))
            ((string= str "//")
             (let ((end (position #\newline text :start pos)))
               (assert end)
               (push (make-instance 'line-comment
                                    :position pos
                                    :string (subseq text pos end))
                     tokens)
               (setf pos (1+ end))))
            ((string= str "'")
             (let ((string-start pos))
               (let ((end (search "'" text :start2 (1+ pos))))
                 (assert end)
                 (push (make-instance 'string-literal
                                      :position pos
                                      :string (subseq text string-start end))
                       tokens)
                 (setf pos (1+ end)))))
            ((digit-char-p (char str 0))
             (push (make-instance 'number-literal
                                  :position pos
                                  :string str)
                   tokens))
            ((word-char-p (char str 0))
             (push (make-instance 'word
                                  :position pos
                                  :string str)
                   tokens))
            (t
             (push (make-instance 'operator
                                  :position pos
                                  :string str)
                   tokens))))
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
   (type :initarg :type)))

(defclass type-or ()
  ((types :initarg :types)))

(defclass simple-type ()
  ((name :initarg :name)))

(defclass array-type ()
  ((name :initarg :name)))

(defclass string-expression ()
  ((value :initarg :value)))

(defclass number-expression ()
  ((value :initarg :value)))

(defclass namespace ()
  ((name :initarg :name)
   (declarations :initarg :declarations)))

(defclass namespace-declaration ()
  ((var :initarg :var)
   (value :initarg :value)
   (type :initarg :type)
   (export-p :initarg :export-p)
   (comment :initarg :comment)))

(defclass type-declaration ()
  ((name :initarg :name)
   (type :initarg :type)))

(defvar *token-iterator*)

(defun next-token ()
  (next *token-iterator*))

(defun ahead-token ()
  (lookahead *token-iterator*))

(defun end-of-token-p ()
  (end-p *token-iterator*))

(defun match (token-type &optional string)
  (let ((token (ahead-token)))
    (when (and (typep token token-type)
               (or (null string)
                   (string= (token-string token) string)))
      token)))

(defun accept (token-type &optional string)
  (when (match token-type string)
    (next-token)))

(defun ts-parse-error ()
  (error 'ts-parse-error
         :position (token-position (ahead-token))
         :message (format nil "Token not expected: ~S" (ahead-token))))

(defun exact (token-type &optional string)
  (or (accept token-type string)
      (ts-parse-error)))

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
                              :value (token-string token))))
           (number-literal ()
             (when-let ((token (accept 'number-literal)))
               (make-instance 'number-expression
                              :value (read-from-string (token-string token)))))
           (type-1 ()
             (cond ((match 'operator "{")
                    (parse-interface-expression))
                   ((type-name))
                   ((string-literal))
                   ((number-literal))))
           (type-or ()
             (let ((types
                     (loop :for type := (type-1)
                           :collect type
                           :while (accept 'operator "|"))))
               (if (length= types 1)
                   (first types)
                   (make-instance 'type-or :types types)))))
    (type-or)))

(defun parse-comments ()
  (flet ((comment-to-string (comment)
           (ppcre:regex-replace-all "\\n\\s*"
                                    (token-string comment)
                                    (string #\newline))))
    (string-trim '(#\newline #\space #\tab)
                 (with-output-to-string (out)
                   (loop :for comment := (accept 'comment)
                         :while comment
                         :do (write-line (comment-to-string comment) out))))))

(defun parse-interface-expression ()
  (exact 'operator "{")
  (let ((elements '()))
    (loop
      (let ((comment (parse-comments)))
        (if-let ((element-name (accept 'word)))
          (let ((optional-p (not (null (accept 'operator "?")))))
            (exact 'operator ":")
            (let ((type (parse-type-expression)))
              (accept 'operator ";")
              (push (make-instance 'element
                                   :name (token-string element-name)
                                   :optional-p optional-p
                                   :type type
                                   :comment comment)
                    elements)))
          (return))))
    (exact 'operator "}")
    (make-instance 'interface :elements (nreverse elements))))

(defun parse-polymorphism-spec ()
  (when (accept 'operator "<")
    (exact 'word)
    (accept 'operator ">")))

(defun parse-def-interface ()
  (when (accept 'word "interface")
    (let ((interface-name (token-string (exact 'word)))
          (* (parse-polymorphism-spec))
          (extends-interface
            (when (accept 'word "extends")
              (token-string (accept 'word)))))
      (let ((interface (parse-interface-expression)))
        (change-class interface
                      'named-interface
                      :name interface-name
                      :extends extends-interface)))))

(defun parse-value ()
  (flet ((parse-number (token)
           (read-from-string (token-string token))))
    (cond ((when-let (token (accept 'string-literal))
             (token-string token)))
          ((when-let (token (accept 'number-literal))
             (parse-number token)))
          ((accept 'operator "-")
           (let ((token (exact 'number-literal)))
             (- (parse-number token))))
          (t
           (ts-parse-error)))))

(defun parse-def-namespace ()
  (when (accept 'word "namespace")
    (let ((namespace-name (token-string (exact 'word))))
      (exact 'operator "{")
      (let ((decls '()))
        (loop
          (let ((comment (parse-comments)))
            (declare (ignore comment))
            (when (accept 'operator "}")
              (return))
            (let ((export-p (not (null (accept 'word "export")))))
              (exact 'word "const")
              (let ((var (token-string (exact 'word)))
                    (type (when (accept 'operator ":")
                            (parse-type-expression))))
                (exact 'operator "=")
                (let ((value (parse-value)))
                  (accept 'operator ";")
                  (push (make-instance 'namespace-declaration
                                       :var var
                                       :value value
                                       :type type
                                       :export-p export-p
                                       #|:comment comment|#)
                        decls))))))
        (make-instance 'namespace
                       :declarations (nreverse decls)
                       :name namespace-name)))))

(defun parse-def-type ()
  (when (accept 'word "type")
    (let ((name (token-string (exact 'word))))
      (exact 'operator "=")
      (let ((type (parse-type-expression)))
        (accept 'operator ";")
        (make-instance 'type-declaration :name name :type type)))))

(defun parse-constant-declaration ()
  ;; export const EOL: string[] = ['\n', '\r\n', '\r'];
  ;; という行をスキップすることしか想定していない
  (when (accept 'word "const")
    (let ((name (accept 'word))
          (type (when (accept 'operator ":")
                  (parse-type-expression))))
      (declare (ignore name type))
      (exact 'operator "=")
      (loop :until (accept 'operator ";")
            :do (next-token)))))

(defun parse (*token-iterator*)
  (loop :while (accept 'comment))
  (accept 'word "export")
  (or (parse-def-interface)
      (parse-def-namespace)
      (parse-def-type)
      (parse-constant-declaration)
      (if (end-of-token-p)
          nil
          (ts-parse-error))))

(defun symbolize (string &optional package)
  (let ((name (string-upcase (cl-change-case:param-case string))))
    (if package
        (intern name package)
        (intern name))))

(defun element-to-slot-specifier (element)
  (with-slots (name optional-p comment type) element
    `(,(if optional-p
           (symbolicate (symbolize name) '?)
           (symbolize name))
      :initarg ,(symbolize name :keyword)
      :documentation ,comment)))

(defgeneric to-lisp (object))

(defmethod to-lisp ((named-interface named-interface))
  (with-slots (name extends elements) named-interface
    `(defclass ,(symbolize name) ,(if extends `(,(symbolize extends)) ())
       ,(mapcar #'element-to-slot-specifier elements))))

(defmethod to-lisp ((namespace namespace))
  (with-slots (name declarations) namespace
    (let ((forms
            (loop :with name := (symbolize name)
                  :for declaration :in declarations
                  :collect (with-slots (var value) declaration
                             `(defparameter ,(symbolicate name #\. (symbolize var)) ,value)))))
      `(progn ,@forms))))

(defmethod to-lisp ((type-declaration type-declaration))
  ;; nop
  )

(defun parse-text (text)
  (let ((token-iterator (tokenize text)))
    (loop :for result := (parse token-iterator)
          :while result
          :collect result)))

(defun print-form (form &optional (stream *standard-output*))
  (let ((*print-case* :downcase)
        (*print-pretty* t))
    (prin1 form stream)))

(defun translate-text (text &optional (stream *standard-output*))
  (dolist (result (parse-text text))
    (fresh-line stream)
    (print-form (to-lisp result) stream)
    (terpri stream)))

(defun translate-file (file &optional (stream *standard-output*))
  (translate-text (read-file-into-string file) stream))

(defun deploy (out-file)
  (with-open-file (out out-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out ";;; Code generated by lem-lsp-mode/typescript; DO NOT EDIT.~%")
    (dolist (file (directory
                   (make-pathname :name :wild
                                  :type "ts"
                                  :defaults (asdf:system-relative-pathname :lem-lsp-mode "./specification/"))))
      (format out "~&~%;; file: ~A~%" file)
      (translate-file file out))))
