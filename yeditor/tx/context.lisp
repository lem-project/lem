(defpackage :yeditor-context
  (:use :cl)
  (:import-from :yeditor-tx
                #:install-effect
                #:*tx-log*)
  (:export #:effect))
(in-package :yeditor-context)

(defun effect (kind owner payload inverse)
  "Thin wrapper calling `install-effect` in yeditor-tx."
  (install-effect kind owner payload inverse))
