(defpackage :lem-background-job-asd
  (:use :cl :asdf))
(in-package :lem-background-job-asd)

(defsystem lem-background-job
  :depends-on (:lem :bordeaux-threads)
  :serial t
  :components ((:file "lem-background-job")))
