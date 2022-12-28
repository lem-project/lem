;;; reimplementation of lem-lsp-server

(defsystem "lem-language-server"
  :depends-on ("jsonrpc"
               "lem-lsp-utils"
               "lem"
               "lem-lisp-syntax")
  :serial t
  :components ((:module "protocol"
                :components ((:file "type")
                             (:file "protocol-generator")
                             ;; (:file "protocol-3-17")
                             (:file "converter")))
               (:file "package")
               ;; (:file "print-object")
               ;; (:file "variables")
               ;; (:file "method")
               ;; (:file "server")
               ;; (:module "methods"
               ;;  :components ((:file "lifecycle")
               ;;               (:file "document-synchronization")))
               ))
