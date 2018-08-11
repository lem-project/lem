;;don't edit
(defsystem "lem-encodings"
  :depends-on (:LEM-BASE)
  :class :package-inferred-system
  :components ((:FILE "iso-8859-1") (:FILE "utf-8") (:FILE "utf-16")))
