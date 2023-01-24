(defpackage :lem-tests/language-server/tests
  (:use :cl
        :testif)
  (:import-from :lem-language-server
                :call-lsp-method
                :start-server)
  (:import-from :lem-language-server/protocol/yason-utils
                :parse-json)
  (:import-from :lem-language-server/protocol/lsp-type
                :make-lsp-map)
  (:import-from :lem-language-server/protocol/converter
                :convert-to-json
                :convert-from-json))
(in-package :lem-tests/language-server/tests)

(test initialize-request-test
  (let ((lem-language-server::*debug-on-error* t)
        (lem-language-server::*server*))
    (start-server (make-instance 'lem-language-server::mock-server))
    (let ((response
            (call-lsp-method (make-instance 'lem-language-server::initialize-request)
                             (convert-to-json
                              (make-instance 'lsp:initialize-params
                                             :process-id (random 10000)
                                             :client-info (make-lsp-map :name "test-client"
                                                                        :version "1.2.3")
                                             :root-uri "file://hoge/piyo/"
                                             :root-path "/hoge/piyo/"
                                             :capabilities (lem-lsp-mode::client-capabilities)
                                             :trace "off"
                                             :workspace-folder :null)))))
      (ok (convert-from-json response 'lsp:initialize-result)))))
