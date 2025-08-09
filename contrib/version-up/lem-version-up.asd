;;don't edit
(defsystem "lem-version-up"
  :depends-on("lem/core" "cl-ppcre")
  :class :package-inferred-system
  :components((:file "version"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")
