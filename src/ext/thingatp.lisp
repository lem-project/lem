(defpackage :lem/thingatp
  (:use :cl :lem))
(in-package :lem/thingatp)

(define-command open-at-point () ()
  (lem/link:link-open))
