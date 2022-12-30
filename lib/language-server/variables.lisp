(in-package :lem-language-server)

(defparameter *language-server-name* "cl-lsp")
(defparameter *language-server-version* "0.1.0")

(defparameter *yason-bindings*
  '((yason:*parse-json-null-as-keyword* . t)
    (yason:*parse-json-arrays-as-vectors* . t)))
