(lem-lsp-server/defpackage:defpackage :lem-lsp-server/test/text-document-did-open
  (:use :cl
        :rove
        :lem-lsp-server/server
        :lem-lsp-server/test/test-server
        :lem-lsp-server/text-document-controller)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol-3-15)
                    (:json :lem-lsp-utils/json)))
(in-package :lem-lsp-server/test/text-document-did-open)

#+(or)
(deftest test
  (let ((server (make-instance 'test-server))
        (whole-text "(defun test (x) (cons x x))"))
    (server-listen server)
    (lem-lsp-server/test/initialize:initialize-request server)
    (let ((response
            (call-lsp-method
             server
             "textDocument/didOpen"
             (json:object-to-json
              (make-instance
               'protocol:did-open-text-document-params
               :text-document (make-instance 'protocol:text-document-item
                                             :uri "file://Users/user/hoge.lisp"
                                             :language-id "lisp"
                                             :version 1
                                             :text whole-text))))))
      (ok (null response))
      (let ((controller (server-text-document-controller server)))
        (let ((text-document (find-text-document controller "file://Users/user/hoge.lisp")))
          (ok text-document)
          (ok (equal whole-text
                     (lem-base:buffer-text (text-document-file-contents text-document)))))))))
