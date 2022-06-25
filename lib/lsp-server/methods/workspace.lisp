(lem-lsp-server/defpackage:defpackage :lem-lsp-server/methods/workspace
  (:use :cl)
  (:import-from :lem-lsp-server/server
                :define-method))
(in-package :lem-lsp-server/methods/workspace)

(define-method "workspace/didChangeConfiguration" () ()
  nil)
