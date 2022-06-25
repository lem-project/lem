(cl-lsp/defpackage:defpackage :cl-lsp/methods/text-document
  (:use :cl
        :cl-lsp/text-document-controller)
  (:import-from :cl-lsp/server
                :define-method
                :this-server
                :server-text-document-controller
                :set-server-text-document-controller)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)
                    (:editor :cl-lsp/editor)))
(in-package :cl-lsp/methods/text-document)

(defun current-controller ()
  (or (server-text-document-controller (this-server))
      (set-server-text-document-controller
       (make-instance 'text-document-controller)
       (this-server))))

(define-method "textDocument/didOpen" (params protocol:did-open-text-document-params) ()
  (let* ((text-document-item (protocol:did-open-text-document-params-text-document params))
         (uri (protocol:text-document-item-uri text-document-item))
         (language-id (protocol:text-document-item-language-id text-document-item))
         (text (protocol:text-document-item-text text-document-item)))
    (open-text-document (current-controller)
                        uri
                        :text text
                        :language-id language-id)
    (values)))

(define-method "textDocument/didChange" (params protocol:did-change-text-document-params) ()
  (let* ((text-document-identifier (protocol:did-change-text-document-params-text-document params))
         (content-changes (protocol:did-change-text-document-params-content-changes params))
         (text-document
           (find-text-document (current-controller)
                               (protocol:text-document-identifier-uri
                                text-document-identifier))))
    (assert text-document)
    (lem-utils:do-sequence (content-change content-changes)
      (apply-content-change (server-text-document-controller(this-server))
                            text-document
                            content-change))
    (values)))

(define-method "textDocument/willSave" () ()
  )

(define-method "textDocument/willSaveWaitUntil" () ()
  )

(define-method "textDocument/didSave" (params protocol:did-save-text-document-params) ()
  )

(define-method "textDocument/didClose" (params protocol:did-close-text-document-params) ()
  (let* ((text-document-identifier (protocol:did-close-text-document-params-text-document params))
         (text-document (find-text-document (current-controller)
                                            (protocol:text-document-identifier-uri
                                             text-document-identifier))))
    (assert text-document)
    (close-text-document (current-controller) text-document)
    (values)))
