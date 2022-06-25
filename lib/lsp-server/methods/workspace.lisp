(cl-lsp/defpackage:defpackage :cl-lsp/methods/workspace
  (:use :cl)
  (:import-from :cl-lsp/server
                :define-method))
(in-package :cl-lsp/methods/workspace)

(define-method "workspace/didChangeConfiguration" () ()
  nil)
