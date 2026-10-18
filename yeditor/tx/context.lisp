(defpackage :yeditor-context
  (:use :cl)
  (:import-from :yeditor-tx
                #:install-effect)
  (:export #:effect))
(in-package :yeditor-context)

(defun effect (kind owner payload inverse)
  "Thin wrapper calling `install-effect` in yeditor-tx.
   Maps the historic arguments to the new `install-effect` signature.
   Returns the handle (log entry)."
  (install-effect kind owner payload :undo-thunk inverse))
