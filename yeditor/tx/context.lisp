(defpackage :yeditor-context-api
  (:use :cl)
  (:import-from :yeditor-tx
   #:install-effect)
  (:export #:effect))
(in-package :yeditor-context-api)

(defun effect (kind owner payload &key undo-thunk)
  "Thin L2 wrapper over yeditor-tx:install-effect (plan Cap. 5.1 context:effect)."
  (install-effect kind owner payload :undo-thunk undo-thunk))
