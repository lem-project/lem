(defpackage :lem-lsp-utils/protocol-generator
  (:use :cl
        :alexandria)
  (:import-from :cl-package-locks)
  (:import-from :lem-lsp-utils/type)
  (:import-from :lem-lsp-utils/json)
  (:import-from :cl-change-case)
  (:import-from :trivial-types)
  (:import-from :cl-ppcre)
  (:export :deploy))
(in-package :lem-lsp-utils/protocol-generator)

(cl-package-locks:lock-package :lem-lsp-utils/protocol-generator)

(define-condition ts-parse-error ()
  ((message :initarg :message
            :reader ts-parse-error-message)
   (position :initarg :position
             :initform nil
             :reader ts-parse-error-position)
   (file-line-number :initarg :file-line-number
                     :initform nil
                     :reader ts-parse-error-file-line-number))
  (:report (lambda (c s)
             (format s
                     "line ~A, offset ~A: ~A"
                     (ts-parse-error-file-line-number c)
                     (ts-parse-error-position c)
                     (ts-parse-error-message c)))))

(defstruct file-part
  text
  line-number)

(defclass token ()
  ((file-line-number :initarg :file-line-number :reader token-file-line-number)
   (string :initarg :string :reader token-string)
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

(defun tokenize (file-part)
  (let ((text (file-part-text file-part))
        (pos 0)
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
                                    :file-line-number (file-part-line-number file-part)
                                    :string (subseq text pos end)
                                    :start-column (get-column text pos))
                     tokens)
               (setf pos (+ end 2))))
            ((string= str "//")
             (let ((end (position #\newline text :start pos)))
               (assert end)
               (push (make-instance 'line-comment
                                    :position pos
                                    :file-line-number (file-part-line-number file-part)
                                    :string (subseq text pos end))
                     tokens)
               (setf pos (1+ end))))
            ((string= str "'")
             (let ((end (search "'" text :start2 pos)))
               (assert end)
               (push (make-instance 'string-literal
                                    :position pos
                                    :file-line-number (file-part-line-number file-part)
                                    :string (subseq text pos end))
                     tokens)
               (setf pos (1+ end))))
            ((digit-char-p (char str 0))
             (push (make-instance 'number-literal
                                  :position pos
                                  :file-line-number (file-part-line-number file-part)
                                  :string str)
                   tokens))
            ((word-char-p (char str 0))
             (push (make-instance 'word
                                  :position pos
                                  :file-line-number (file-part-line-number file-part)
                                  :string str)
                   tokens))
            (t
             (push (make-instance 'operator
                                  :position pos
                                  :file-line-number (file-part-line-number file-part)
                                  :string str)
                   tokens))))
    (make-instance 'iterator :list (nreverse tokens))))

(defclass interface ()
  ((elements :initarg :elements :reader interface-elements)))

(defclass named-interface (interface)
  ((name :initarg :name)
   (extends :initarg :extends)))

(defclass element ()
  ((name :initarg :name :reader element-name)
   (optional-p :initarg :optional-p :reader element-optional-p)
   (read-only-p :initarg :read-only-p :reader element-read-only-p)
   (comment :initarg :comment :reader element-comment)
   (type :initarg :type :reader element-type)))

(defclass type-or ()
  ((types :initarg :types :reader type-or-types)))

(defclass simple-type ()
  ((name :initarg :name :reader simple-type-name)))

(defclass array-type ()
  ((item-type :initarg :item-type :reader array-type-item-type)))

(defclass value-expression ()
  ((value :initarg :value :reader value-expression-value)))

(defclass string-expression (value-expression)
  ())

(defclass number-expression (value-expression)
  ())

(defclass property-type ()
  ((key-type :initarg :key-type :reader property-type-key-type)
   (value-type :initarg :value-type :reader property-type-value-type)))

(defclass tuple-type ()
  ((types :initarg :types :reader tuple-type-types)))

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
  ((name :initarg :name :reader type-declaration-name)
   (type :initarg :type :reader type-declaration-type)))

(defclass enum ()
  ((name :initarg :name)
   (declarations :initarg :declarations)))

(defclass enum-declarations ()
  ((name :initarg :name)
   (value :initarg :value)
   (comment :initarg :comment)))

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
         :file-line-number (token-file-line-number (ahead-token))
         :message (format nil "Token not expected: ~S" (ahead-token))))

(defun exact (token-type &optional string)
  (or (accept token-type string)
      (ts-parse-error)))

(defun parse-type-expression ()
  (labels ((array-suffix (type)
             (cond ((accept 'operator "[")
                    (exact 'operator)
                    (make-instance 'array-type :item-type type))
                   (t
                    type)))
           (type-name ()
             (when-let ((name (accept 'word)))
               (array-suffix (make-instance 'simple-type :name (token-string name)))))
           (string-literal ()
             (when-let ((token (accept 'string-literal)))
               (make-instance 'string-expression
                              :value (token-string token))))
           (number-literal ()
             (when-let ((token (accept 'number-literal)))
               (make-instance 'number-expression
                              :value (read-from-string (token-string token)))))
           (array-literal ()
             (when (accept 'operator "[")
               (make-instance 'tuple-type
                              :types (if (accept 'operator "]")
                                         '()
                                         (loop
                                           :collect (or-expr)
                                           :until (accept 'operator "]")
                                           :do (exact 'operator ","))))))
           (primary ()
             (cond ((accept 'operator "(")
                    (let ((type (or-expr)))
                      (exact 'operator ")")
                      (array-suffix type)))
                   ((match 'operator "{")
                    (parse-interface-expression))
                   ((type-name))
                   ((string-literal))
                   ((number-literal))
                   ((array-literal))
                   (t (ts-parse-error))))
           (or-expr ()
             (let ((types
                     (loop :collect (primary)
                           :while (accept 'operator "|"))))
               (if (length= types 1)
                   (first types)
                   (make-instance 'type-or :types types)))))
    (or-expr)))

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
  (labels ((parse-property (comment read-only-p)
             (exact 'operator "[")
             (let ((name (exact 'word)))
               (exact 'operator ":")
               (let ((key-type (parse-type-expression)))
                 (exact 'operator "]")
                 (exact 'operator ":")
                 (let ((value-type (parse-type-expression)))
                   (make-instance 'element
                                  :name (print (token-string name))
                                  :optional-p t
                                  :read-only-p read-only-p
                                  :type (make-instance 'property-type
                                                       :key-type key-type
                                                       :value-type value-type)
                                  :comment comment)))))
           (parse-simple-element (comment read-only-p)
             (if-let ((element-name (accept 'word)))
               (let ((optional-p (not (null (accept 'operator "?")))))
                 (exact 'operator ":")
                 (let ((type (parse-type-expression)))
                   (make-instance 'element
                                  :name (token-string element-name)
                                  :optional-p optional-p
                                  :read-only-p read-only-p
                                  :type type
                                  :comment comment)))))
           (parse-element ()
             (let ((comment (parse-comments))
                   (read-only-p (not (null (accept 'word "readonly")))))
               (if (match 'operator "[")
                   (parse-property comment read-only-p)
                   (parse-simple-element comment read-only-p)))))
    (exact 'operator "{")
    (let ((elements '()))
      (loop
        (if-let ((element (parse-element)))
          (progn
            (accept 'operator ";")
            (push element elements))
          (return)))
      (exact 'operator "}")
      (make-instance 'interface :elements (nreverse elements)))))

(defun parse-polymorphism-spec ()
  (when (accept 'operator "<")
    (exact 'word)
    (accept 'operator ">")))

(defun parse-extends-names ()
  (loop
    :collect (token-string (exact 'word))
    :while (accept 'operator ",")))

(defun parse-def-interface ()
  (when (accept 'word "interface")
    (let ((interface-name (token-string (exact 'word)))
          (* (parse-polymorphism-spec))
          (extends-interface
            (when (accept 'word "extends")
              (parse-extends-names))))
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

(defun parse-def-enum ()
  (flet ((parse-enum-value ()
           (let ((token (exact 'string-literal)))
             (make-instance 'string-expression :value (token-string token)))))
    (when (accept 'word "enum")
      (let ((name (token-string (exact 'word)))
            (declarations '()))
        (exact 'operator "{")
        (loop
          (let ((comment (parse-comments))
                (name (token-string (exact 'word)))
                (value (if (accept 'operator "=")
                           (parse-enum-value)
                           nil)))
            (push (make-instance 'enum-declarations
                                 :name name
                                 :value value
                                 :comment comment)
                  declarations)
            (if (accept 'operator "}")
                (return)
                (exact 'operator ","))))
        (make-instance 'enum
                       :name name
                       :declarations (nreverse declarations))))))

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
      (parse-def-enum)
      (parse-constant-declaration)
      (if (end-of-token-p)
          nil
          (ts-parse-error))))

(defun parse-text (file-part)
  (let ((token-iterator (tokenize file-part)))
    (loop :for result := (parse token-iterator)
          :while result
          :collect result)))

(defvar *export-list* '())

(defun add-export (name)
  (pushnew name *export-list*))

(defgeneric to-lisp (object))
(defgeneric to-lisp-type (type))

(defun symbolize (string &optional package)
  (let ((name (string-upcase (cl-change-case:param-case string))))
    (if package
        (intern name package)
        (intern name))))

(defmethod to-lisp-type ((type simple-type))
  (switch ((simple-type-name type) :test #'string=)
    ("any" t)
    ("boolean" 'lem-lsp-utils/type:ts-boolean)
    ("number" 'number)
    ("string" 'string)
    ("null" '(or null (eql :null))) ; TODO: jsonライブラリ毎の:nullやnilという違いを考慮する
    (otherwise
     (symbolize (simple-type-name type)))))

(defmethod to-lisp-type ((type type-or))
  `(or ,@(mapcar #'to-lisp-type (type-or-types type))))

(defmethod to-lisp-type ((type array-type))
  (let ((item-type (to-lisp-type (array-type-item-type type))))
    `(lem-lsp-utils/type:ts-array ,item-type)))

(defmethod to-lisp-type ((type value-expression))
  `(lem-lsp-utils/type:ts-equal-specializer ,(value-expression-value type)))

(defmethod to-lisp-type ((type interface))
  `(lem-lsp-utils/type:ts-interface
    ,@(mapcar (lambda (element)
                (list (element-name element)
                      :type (to-lisp-type (element-type element))
                      :optional-p (element-optional-p element)))
              (interface-elements type))))

(defmethod to-lisp-type ((type property-type))
  `(lem-lsp-utils/type:ts-object ,(to-lisp-type (property-type-key-type type))
                                ,(to-lisp-type (property-type-value-type type))))

(defmethod to-lisp-type ((type tuple-type))
  `(lem-lsp-utils/type:ts-tuple ,@(mapcar #'to-lisp-type (tuple-type-types type))))

(defun element-to-slot-specifier (class-name element)
  (with-slots (name optional-p comment type) element
    (let ((reader (symbolicate class-name '- (symbolize name))))
      (add-export reader)
      `(,(if optional-p
             (symbolicate (symbolize name) '?)
             (symbolize name))
        :initarg ,(symbolize name :keyword)
        :documentation ,comment
        :type ,(to-lisp-type type)
        :reader ,reader))))

(defmethod to-lisp ((named-interface named-interface))
  (with-slots (name extends elements) named-interface
    (let ((class-name (symbolize name)))
      (add-export class-name)
      `(defclass ,class-name
           ,(if extends
                (mapcar #'symbolize extends)
                '(lem-lsp-utils/json:object))
         ,(mapcar (curry #'element-to-slot-specifier class-name) elements)))))

(defmethod to-lisp ((namespace namespace))
  (with-slots (name declarations) namespace
    (let ((forms
            (loop :with name := (symbolize name)
                  :for declaration :in declarations
                  :collect (with-slots (var value) declaration
                             (let ((name (symbolicate name #\. (symbolize var))))
                               (add-export name)
                               `(defparameter ,name ,value))))))
      `(progn
         (deftype ,(symbolize name) () t)
         ,@forms))))

(defmethod to-lisp ((enum enum))
  ;; TODO
  )

(defmethod to-lisp ((type-declaration type-declaration))
  `(deftype ,(symbolize (type-declaration-name type-declaration)) ()
     ',(to-lisp-type (type-declaration-type type-declaration))))

(defun print-form (form &optional (stream *standard-output*))
  (let ((*print-pretty* t))
    (prin1 form stream)))

(defun ensure-protocol-package ()
  (or (find-package :lem-lsp-utils/protocol)
      (make-package :lem-lsp-utils/protocol :use '())))

(defun translate-text (file-part &optional (stream *standard-output*))
  (loop :with protocol-package := (ensure-protocol-package)
        :for result :in (parse-text file-part)
        :do (let ((*package* protocol-package))
              (fresh-line stream)
              (terpri stream)
              (print-form (to-lisp result) stream)
              (terpri stream))))

(defun extract-typescript (spec-file)
  (flet ((lines-to-string (lines)
           (with-output-to-string (out)
             (dolist (line lines)
               (write-line line out)))))
    (with-open-file (in spec-file)
      (let ((code-list '()))
        (loop :with in-code-p := nil
              :and lines := '()
              :and start-line-number := nil
              :for line := (read-line in nil nil)
              :for line-number :from 1
              :while line
              :do (let ((line (string-right-trim '(#\Return) line)))
                    (if in-code-p
                        (cond ((ppcre:scan "^```" line)
                               (setf in-code-p nil)
                               (push (make-file-part :line-number (1+ start-line-number)
                                                     :text (lines-to-string (nreverse lines)))
                                     code-list))
                              (t
                               (push line lines)))
                        (when (ppcre:scan "^```typescript" line)
                          (setf in-code-p t)
                          (setf lines '())
                          (setf start-line-number line-number)))))
        (nreverse code-list)))))

(defun generate (spec-file out-file)
  (with-open-file (out out-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let* ((*export-list* '())
           (body-text
             (with-output-to-string (body-out)
               (dolist (file-part (extract-typescript spec-file))
                 (handler-case (translate-text file-part body-out)
                   (ts-parse-error (c)
                     (warn "~A" c)))))))
      (format out ";;; Code generated by ~A based on ~A; DO NOT EDIT.~%"
              :lem-lsp-utils/protocol-generator
              (make-pathname :name (pathname-name spec-file)
                             :type (pathname-type spec-file)))
      (pprint `(defpackage :lem-lsp-utils/protocol
                 (:import-from :lem-lsp-utils/json)
                 (:import-from :lem-lsp-utils/type)
                 (:export ,@(mapcar #'make-keyword *export-list*)))
              out)
      (pprint '(in-package :lem-lsp-utils/protocol) out)
      (format out "~&~A" "(cl-package-locks:lock-package :lem-lsp-utils/protocol)")
      (write-string body-text out)))
  (values))

(defun deploy ()
  (generate (asdf:system-relative-pathname :lem-lsp-utils "specification/specification-3-15.md")
            (asdf:system-relative-pathname :lem-lsp-utils "protocol.lisp")))
