(defpackage :lem-lsp-utils/meta-model
  (:use :cl :alexandria)
  (:import-from :cl-change-case))
(in-package :lem-lsp-utils/meta-model)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; types
(deftype lsp-uri () t)
(deftype lsp-document-uri () t)
(deftype lsp-integer () t)
(deftype lsp-uinteger () t)
(deftype lsp-decimal () t)
(deftype lsp-regexp () t)
(deftype lsp-string () t)
(deftype lsp-boolean () t)
(deftype lsp-null () '(eql :null))

(deftype lsp-array (&optional (element-type *))
  (declare (ignore element-type))
  t)

(deftype lsp-map (key value)
  (declare (ignore key value))
  t)

(deftype lsp-tuple (&rest types)
  (declare (ignore types))
  t)

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

(defun symbolize (string &optional package)
  (let ((name (string-upcase (cl-change-case:param-case string))))
    (if package
        (intern name package)
        (intern name))))

;;; parser
(defun parse-and-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (items "items" :required t))
      hash
    (assert (equal kind "and"))
    `(and ,@(mapcar #'parse-type items))))

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
  )

(defun parse-enumeration-entry (hash)
  (check-type hash hash-table)
  )

(defun parse-enumeration-type (hash)
  (check-type hash hash-table)
  )

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
  (assert (member string '("clientToServer"
                           "serverToClient"
                           "both")
                  :test #'string=))
  ;; TODO
  )

(defun parse-metadata (hash)
  (check-type hash hash-table)
  )

(defun parse-meta-model (hash)
  (check-type hash hash-table)
  )

(defun parse-notification (hash)
  (check-type hash hash-table)
  )

(defun parse-or-type (hash)
  (check-type hash hash-table)
  (with-hash ((items "items" :required t)
              (kind "kind" :required t))
      hash
    (assert (string= kind "or"))
    `(or ,@(mapcar #'parse-type items))))

(defun parse-property (hash)
  (check-type hash hash-table)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (name "name" :required t)
              (optional "optional")
              (proposed "proposed")
              (since "since")
              (type "type" :required t))
      hash
    (declare (ignore documentation))
    `(,(symbolize name)
      :type ,(parse-type type)
      ;; ,@(when documentation `(:documentation ,documentation))
      ,@(when deprecated `(:deprecated ,deprecated))
      ,@(when optional `(:optional t))
      ,@(when proposed `(:proposed t))
      ,@(when since `(:since ,since)))))

(defun parse-reference-type (hash)
  (check-type hash hash-table)
  (with-hash ((kind "kind" :required t)
              (name "name" :required t))
      hash
    (assert (string= kind "reference"))
    (symbolize name)))

(defun parse-request (hash)
  (check-type hash hash-table)
  )

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
  )

(defun parse-structure-literal (hash)
  (check-type hash hash-table)
  )

(defun parse-structure-literal-type (hash)
  (check-type hash hash-table)
  )

(defun parse-tuple-type (hash)
  (check-type hash hash-table)
  (with-hash ((items "items" :required t)
              (kind "kind" :required t))
      hash
    (assert (string= kind "tuple"))
    (let ((items (mapcar #'parse-type items)))
      `(ts-tuple ,@items))))

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
  )

(defun parse-type-kind (hash)
  (check-type hash hash-table)
  )

;;; read metaModel.json
(defun read-structure (hash)
  (with-hash ((deprecated "deprecated")
              (documentation "documentation")
              (extends "extends")
              (mixins "mixins")
              (name "name" :required t)
              (properties "properties" :required t)
              (proposed "proposed")
              (since "since"))
      hash
    (declare (ignore documentation))
    `(define-structure-protocol ,(symbolize name) (,@(mapcar #'parse-type extends)
                                                   ,@(mapcar #'parse-type mixins))
       ,(mapcar #'parse-property properties)
       ,@(when deprecated `(:deprecated ,deprecated))
       ;; ,@(when documentation `(:documentation ,documentation))
       ,@(when proposed `(:proposed ,proposed))
       ,@(when since `(:since ,since)))))

(defun parse ()
  (yason:parse (asdf:system-relative-pathname
                :lem-lsp-utils
                "specification/language-server-protocol/_specifications/lsp/3.17/metaModel/metaModel.json")
               :json-nulls-as-keyword t))
