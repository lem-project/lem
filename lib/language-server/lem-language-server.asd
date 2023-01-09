;;; reimplementation of lem-lsp-server

(require :sb-concurrency)

(defsystem "lem-language-server"
  :depends-on ("jsonrpc"
               "lem-lsp-utils"
               "lem"
               "lem-lisp-syntax"
               "async-process"
               "quri"
               "micros/client")
  :serial t
  :components ((:module "protocol"
                :components ((:file "yason-utils")
                             (:file "type")
                             (:file "protocol-generator")
                             (:file "protocol-3-17")
                             (:file "converter")
                             (:file "uri")))
               (:file "package")
               (:file "variables")
               (:file "editor-utils")
               (:file "method")
               (:file "server")
               (:file "text-document")
               (:file "converter")
               (:module "methods"
                :components ((:file "lifecycle")
                             (:file "document-synchronization")
                             (:file "language-features")))))
