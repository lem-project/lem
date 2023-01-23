;;; reimplementation of lem-lsp-server

(require :sb-concurrency)

(defsystem "lem-language-server"
  :depends-on ("alexandria"
               "jsonrpc"
               "usocket"
               "log4cl"
               "quri"
               "cl-change-case"
               "async-process"
               "micros"
               "lem"
               "lem-lisp-syntax"
               "lem-socket-utils")
  :serial t
  :components ((:module "protocol"
                :components ((:file "yason-utils")
                             (:file "lsp-type")
                             (:file "protocol-generator")
                             (:file "protocol-3-17")
                             (:file "converter")
                             (:file "utils")))
               (:file "micros-client")
               (:file "package")
               (:file "config")
               (:file "editor-utils")
               (:file "method")
               (:file "server")
               (:file "runtime")
               (:file "text-document")
               (:file "commands")
               (:module "methods"
                :components ((:file "lifecycle")
                             (:file "document-synchronization")
                             (:file "language-features")
                             (:file "workspace")))))
