(in-package :lem-language-server)

(define-request (text-document-did-open "textDocument/didOpen")
    (params lsp:did-open-text-document-params)
  (with-accessors ((item lsp:did-open-text-document-params-text-document))
      params
    (with-accessors ((uri lsp:text-document-item-uri)
                     (language-id lsp:text-document-item-language-id)
                     (version lsp:text-document-item-version)
                     (text lsp:text-document-item-text))
        item
      (let ((buffer (lem:make-buffer (format nil "*lsp ~A ~A*" uri version)
                                     :enable-undo-p nil
                                     :syntax-table lem-lisp-syntax:*syntax-table*)))
        (lem:insert-string (lem:buffer-point buffer) text)
        (register-text-document :uri uri
                                :language-id language-id
                                :version version
                                :buffer buffer))
      (values))))

(define-request (text-document-did-change "textDocument/didChange")
    (params lsp:did-change-text-document-params)
  (with-accessors ((text-document-identifier lsp:did-change-text-document-params-text-document)
                   (content-changes lsp:did-change-text-document-params-content-changes))
      params
    (let ((text-document (find-text-document text-document-identifier)))
      (lem:do-sequence (content-change content-changes)
        (edit-text-document text-document content-change))))
  (values))

;; TODO
(define-request (text-document-will-save "textDocument/willSave")
    (params lsp:will-save-text-document-params)
  (declare (ignore params))
  )

;; TODO
(define-request (text-document-will-save-wait-until "textDocument/willSaveWaitUntil")
    (params lsp:will-save-text-document-params)
  (declare (ignore params))
  )

;; TODO
(define-request (text-document-did-save "textDocument/didSave")
    (params lsp:did-save-text-document-params)
  (declare (ignore params))
  )

(define-request (text-document-did-close "textDocument/didClose")
    (params lsp:did-close-text-document-params)
  (with-accessors ((text-document-identifier lsp:did-close-text-document-params-text-document))
      params
    (let ((text-document (find-text-document text-document-identifier)))
      (close-text-document text-document)))
  (values))
