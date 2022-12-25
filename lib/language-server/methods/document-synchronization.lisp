(in-package :lem-language-server)

#+TODO
(define-request (text-document-did-open "textDocument/didOpen")
    (params protocol:did-open-text-document-params)
  )

#+TODO
(define-request (text-document-did-change "textDocument/didChange")
    (params protocol:did-change-text-document-params)
  )

#+TODO
(define-request (text-document-will-save "textDocument/willSave")
    (params protocol:will-save-text-document-params)
  )

#+TODO
(define-request (text-document-will-save-wait-until "textDocument/willSaveWaitUntil")
    (params protocol:will-save-text-document-params)
  )

#+TODO
(define-request (text-document-did-save "textDocument/didSave")
    (params protocol:did-save-text-document-params)
  )

#+TODO
(define-request (text-document-did-close "textDocument/didClose")
    (params protocol:did-close-text-document-params)
  )
