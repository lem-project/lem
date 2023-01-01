;;; reimplementation of lem-lsp-server

(require :sb-concurrency)

(defsystem "lem-language-server"
  :depends-on ("jsonrpc"
               "lem-lsp-utils"
               "lem"
               "lem-lisp-syntax")
  :serial t
  :components ((:module "protocol"
                :components ((:file "type")
                             (:file "protocol-generator")
                             (:file "protocol-3-17")
                             (:file "converter")))
               (:file "package")
               (:file "variables")
               (:file "method")
               (:file "server")
               (:file "text-document")
               (:module "methods"
                :components ((:file "lifecycle")
                             (:file "document-synchronization")
                             (:file "language-features")))))

(defsystem "lem-language-server/internal-rpc/server"
  :depends-on ()
  :serial t
  :components ((:module "internal-rpc"
                :components ((:file "rpc")
                             (:file "server")))))

(defsystem "lem-language-server/internal-rpc/client"
  :depends-on ("async-process" "log4cl")
  :serial t
  :components ((:module "internal-rpc"
                :components ((:file "rpc")
                             (:file "client")))))
