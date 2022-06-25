(cl-lsp/defpackage:defpackage :cl-lsp/text-document-controller
  (:use :cl)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)
                    (:editor :cl-lsp/editor))
  (:export :text-document-uri
           :text-document-language-id
           :text-document-file-contents
           :text-document-controller
           :find-text-document
           :open-text-document
           :close-text-document
           :apply-content-change))
(in-package :cl-lsp/text-document-controller)

(defstruct text-document
  uri
  language-id
  file-contents)

(defclass text-document-controller ()
  ((items :initform '()
          :accessor text-document-controller-items)))

(defmethod find-text-document ((this text-document-controller) uri)
  (find uri (text-document-controller-items this)
        :test #'string=
        :key #'text-document-uri))

(defmethod open-text-document ((this text-document-controller) uri &key (text "") language-id)
  (check-type text string)
  (when (find-text-document this uri)
    (error "The file is already opened: ~S" uri))
  (let ((file-contents (editor:open-file-contents uri text)))
    (push (make-text-document :uri uri
                              :language-id language-id
                              :file-contents file-contents)
          (text-document-controller-items this))
    file-contents))

(defmethod close-text-document ((this text-document-controller) text-document)
  (editor:close-file-contents (text-document-file-contents text-document))
  (alexandria:deletef (text-document-controller-items this) text-document)
  (values))

(defun convert-position (position)
  (editor:make-file-contents-position
   :line (protocol:position-line position)
   :character (protocol:position-character position)))

(defun convert-range (range)
  (editor:make-file-contents-range
   :start (convert-position (protocol:range-start range))
   :end (convert-position (protocol:range-end range))))

(defmethod apply-content-change ((this text-document-controller) text-document content-change)
  (cond ((stringp content-change)
         (editor:replace-file-contents text-document content-change))
        (t
         (let ((range (json:json-get content-change "range"))
               (text (json:json-get content-change "text")))
           (editor:edit-file-contents (text-document-file-contents text-document)
                                      (convert-range range)
                                      text)))))
