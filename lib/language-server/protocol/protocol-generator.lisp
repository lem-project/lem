(defpackage :lem-language-server/protocol-generator
  (:use :cl
        :alexandria
        :lem-language-server/type)
  (:export :deploy))
(in-package :lem-language-server/protocol-generator)

(defparameter *specifications*
  '(("language-server-protocol/_specifications/lsp/3.17/metaModel/metaModel.json"
     "protocol/protocol-3-17.lisp"
     :lem-language-server/protocol-3-17)))

(defvar *protocol-package*)
(defvar *exports* '())

;;; utils
(defun exists-key-p (key hash)
  (let ((default '#:default))
    (not (eq default (gethash key hash default)))))

(defun exists-keys-p (hash &rest keys)
  (loop :for key :in keys
        :always (exists-key-p key hash)))

(defun gethash* (key hash-table)
  (let* ((default '#:default)
         (value (gethash key hash-table default)))
    (assert (not (eq value default)))
    value))

(defmacro with-hash ((&rest bindings) hash-table &body body)
  (once-only (hash-table)
    `(progn
       (let ,(loop :for (var key &key required) :in bindings
                   :collect `(,var ,(if required
                                        `(gethash* ,key ,hash-table)
                                        `(gethash ,key ,hash-table))))
         ,@body))))

(defun pascal-to-lisp-case (string)
  (string-upcase
   (if (starts-with-subseq "_" string)
       (uiop:strcat "_" (cl-change-case:param-case string))
       (cl-change-case:param-case string))))

(defun lisp-to-pascal-case (string)
  (if (starts-with-subseq "_" string)
      (uiop:strcat "_" (cl-change-case:pascal-case string))
      (cl-change-case:pascal-case string)))

(defun symbolize (string &optional (package *protocol-package*))
  (let ((name (pascal-to-lisp-case string)))
    (if package
        (intern name package)
        (intern name))))

(defun unimplemented ()
  (error "unimplemented"))

(defun add-export (name)
  (pushnew name *exports*))

;;; parser
(defun parse-and-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (items "items" :required t))
      hash
    (assert (equal kind "and"))
    `(and ,@(map 'list #'parse-type items))))

(defun parse-array-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (element "element" :required t))
      hash
    (assert (equal kind "array"))
    (let ((element-type (parse-type element)))
      `(lsp-array ,element-type))))

(defun parse-base-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (name "name" :required t))
      hash
    (assert (equal kind "base"))
    (parse-base-types name)))

(defun parse-base-types (string)
  (check-type string string)
  (eswitch (string :test #'string=)
    ("URI" 'lsp-uri)
    ("DocumentUri" 'lsp-document-uri)
    ("integer" 'lsp-integer)
    ("uinteger" 'lsp-uinteger)
    ("decimal" 'lsp-decimal)
    ("RegExp" 'lsp-regexp)
    ("string" 'lsp-string)
    ("boolean" 'lsp-boolean)
    ("null" 'lsp-null)))

(defun parse-boolean-literal-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (value "value" :required t))
      hash
    (assert (equal kind "booleanLiteral"))
    (check-type value boolean)
    value))

(defun parse-enumeration (hash)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (name "name" :required t)
              (proposed "proposed")
              (since "since")
              (supports-custom-values "supports-custom-values")
              (type "type" :required t)
              (values "values" :required t))
      hash
    (declare (ignore supports-custom-values))
    (let ((enum-name (symbolize name)))
      (add-export enum-name)
      `(define-enum ,enum-name
         ,(map 'list #'parse-enumeration-entry values)
         (:type ,(parse-enumeration-type type))
         ,@(when deprecated `(:since ,deprecated))
         ,@(when documentation `(:since ,documentation))
         ,@(when proposed `(:since ,proposed))
         ,@(when since `(:since ,since))))))

(defun parse-enumeration-entry (hash)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (name "name" :required t)
              (proposed "proposed")
              (since "since")
              (value "value" :required t))
      hash
    (let ((field-name (symbolize name)))
      (add-export field-name)
      `(,field-name ,value
                    ,@(when deprecated `(:deprecated ,deprecated))
                    ,@(when documentation `(:documentation ,documentation))
                    ,@(when proposed `(:proposed ,proposed))
                    ,@(when since `(:since ,since))))))

(defun parse-enumeration-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (name "name" :required t))
      hash
    (assert (string= kind "base"))
    (eswitch (name :test #'string=)
      ("string" 'lsp-string)
      ("integer" 'lsp-integer)
      ("uinteger" 'lsp-uinteger))))

(defun parse-integer-literal-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (value "value" :required t))
      hash
    (assert (string= kind "integerLiteral"))
    (check-type value integer)
    value))

(defun parse-map-key-type (hash)
  (check-type hash hash-table)
  (if (exists-keys-p hash "kind" "name")
      (with-hash ((kind "kind" :required t)
                  (name "name" :required t))
          hash
        (eswitch (kind :test #'string=)
          ("base"
           (assert (member name '("URI" "DocumentUri" "string" "integer") :test #'string=))
           (symbolize name))
          ("reference"
           (symbolize name))))
      (parse-reference-type hash)))

(defun parse-map-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (key "key" :required t)
              (value "value" :required t))
      hash
    (assert (string= kind "map"))
    (let ((key-type (parse-map-key-type key))
          (value-type (parse-type value)))
      `(lsp-map ,key-type ,value-type))))

(defun parse-message-direction (string)
  (check-type string string)
  (assert (member string
                  '("clientToServer"
                    "serverToClient"
                    "both")
                  :test #'string=))
  (unimplemented))

(defun parse-meta-data (hash)
  (check-type hash hash-table)
  (with-hash ((version "version" :required t))
      hash
    version))

(defun parse-notification (hash)
  (check-type hash hash-table)
  (unimplemented))

(defun parse-or-type (hash)
  (check-type hash hash-table)
  (with-hash ((items "items" :required t)
              (kind "kind" :required t))
      hash
    (assert (string= kind "or"))
    `(or ,@(map 'list #'parse-type items))))

(defun parse-property (hash &key class-name)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (name "name" :required t)
              (optional "optional")
              (proposed "proposed")
              (since "since")
              (type "type" :required t))
      hash
    (let* ((property-name (symbolize name))
           (accessor (when class-name
                       (intern (format nil "~A-~A" class-name property-name)
                               *protocol-package*))))
      (when class-name (add-export accessor))
      `(,property-name
        :type ,(parse-type type)
        ,@(when class-name
            `(:initarg ,(make-keyword property-name)
              :accessor ,accessor))
        ,@(when optional `(:optional ,optional))
        ,@(when deprecated `(:deprecated ,deprecated))
        ,@(when proposed `(:proposed t))
        ,@(when since `(:since ,since))
        ,@(when documentation `(:documentation ,documentation))))))

(defun parse-reference-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (name "name" :required t))
      hash
    (assert (string= kind "reference"))
    (symbolize name)))

(defun parse-request (hash)
  (check-type hash hash-table)
  (unimplemented))

(defun parse-string-literal-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (value "value" :required t))
      hash
    (assert (string= kind "stringLiteral"))
    (check-type value string)
    value))

(defun parse-structure (hash)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (extends "extends")
              (mixins "mixins")
              (name "name" :required t)
              (properties "properties" :required t)
              (proposed "proposed")
              (since "since"))
      hash
    (let* ((structure-name (symbolize name))
           (slot-definitions (map 'list
                                  (rcurry #'parse-property :class-name structure-name)
                                  properties)))
      (add-export structure-name)
      (loop :for (slot-name) :in slot-definitions
            :do (add-export slot-name))
      `(define-class ,structure-name (,@(map 'list #'parse-type extends)
                                      ,@(map 'list #'parse-type mixins))
           ,slot-definitions
         ,@(when deprecated `((:deprecated ,deprecated)))
         ,@(when proposed `((:proposed ,proposed)))
         ,@(when since `((:since ,since)))
         ,@(when documentation `((:documentation ,documentation)))))))

(defun parse-structure-literal (hash)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (properties "properties" :required t)
              (proposed "proposed")
              (since "since"))
      hash
    (check-type properties vector)
    `(lsp-interface ,(map 'list #'parse-property properties)
                    ,@(when deprecated `(:deprecated ,deprecated))
                    ,@(when documentation `(:documentation ,documentation))
                    ,@(when proposed `(:proposed ,proposed))
                    ,@(when since `(:since ,since)))))

(defun parse-structure-literal-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (value "value" :required t))
      hash
    (assert (string= kind "literal"))
    (parse-structure-literal value)))

(defun parse-tuple-type (hash)
  (check-type hash hash-table)
  (with-hash ((items "items" :required t)
              (kind "kind" :required t))
      hash
    (assert (string= kind "tuple"))
    (let ((items (map 'list #'parse-type items)))
      `(lsp-tuple ,@items))))

(defun parse-type (hash)
  (check-type hash hash-table)
  (eswitch ((gethash* "kind" hash) :test #'string=)
    ("base"
     (parse-base-type hash))
    ("reference"
     (parse-reference-type hash))
    ("array"
     (parse-array-type hash))
    ("map"
     (parse-map-type hash))
    ("and"
     (parse-and-type hash))
    ("or"
     (parse-or-type hash))
    ("tuple"
     (parse-tuple-type hash))
    ("literal"
     (parse-structure-literal-type hash))
    ("stringLiteral"
     (parse-string-literal-type hash))
    ("integerLiteral"
     (parse-integer-literal-type hash))
    ("booleanLiteral"
     (parse-boolean-literal-type hash))))

(defun parse-type-alias (hash)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (name "name" :required t)
              (proposed "proposed")
              (since "since")
              (type "type" :required t))
      hash
    (let ((alias-name (symbolize name)))
      (add-export alias-name)
      `(define-type-alias ,alias-name ,(parse-type type)
         ,@(when deprecated `((:deprecated ,deprecated)))
         ,@(when documentation `((:documentation ,documentation)))
         ,@(when proposed `((:proposed ,proposed)))
         ,@(when since `((:since ,since)))))))

(defun read-meta-model (meta-model-file)
  (yason:parse meta-model-file
               :json-nulls-as-keyword t
               :json-arrays-as-vectors t))

(defun pretty-print (form &optional (stream *standard-output*))
  (let ((*print-case* :downcase)
        (*print-right-margin* 100)
        (*package* *protocol-package*))
    (pprint form stream)))

(defun newline-and-pretty-print (form &optional (stream *standard-output*))
  (terpri stream)
  (pretty-print form stream))

(defun find-or-make-package (package-name)
  (or (find-package package-name)
      (make-package package-name :use '(:lem-language-server/type))))

(defun generate (meta-model-file output-file package-name)
  (with-hash ((enumerations "enumerations" :required t)
              (meta-data "metaData" :required t)
              (notifications "notifications" :required t) ; TODO
              (requests "requests" :required t)           ; TODO
              (structures "structures" :required t)
              (type-aliases "typeAliases" :required t))
      (read-meta-model meta-model-file)
    (declare (ignore requests notifications))
    (let* ((*protocol-package* (find-or-make-package package-name))
           (*exports* '(:*version*))
           (version (parse-meta-data meta-data))
           (enumerations (map 'list #'parse-enumeration enumerations))
           (structures (map 'list #'parse-structure structures))
           (type-aliases (map 'list #'parse-type-alias type-aliases)))
      (declare (ignore version))
      (with-open-file (output-stream output-file
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
        (format output-stream
                ";;; Code generated based on '~A'; DO NOT EDIT.~%"
                meta-model-file)
        (pretty-print `(defpackage ,package-name
                         (:use :lem-language-server/type)
                         (:export . ,(mapcar #'make-keyword (nreverse *exports*))))
                      output-stream)
        (pretty-print `(in-package ,package-name) output-stream)
        (mapc (rcurry #'newline-and-pretty-print output-stream) enumerations)
        (mapc (rcurry #'newline-and-pretty-print output-stream) structures)
        (mapc (rcurry #'newline-and-pretty-print output-stream) type-aliases))))
  (format t "~&generated ~S~%" output-file)
  (values))

(defun deploy ()
  (loop :for (meta-model-file output-file package-name) :in *specifications*
        :do (generate (asdf:system-relative-pathname :lem meta-model-file)
                      (asdf:system-relative-pathname :lem-language-server output-file)
                      package-name)))
