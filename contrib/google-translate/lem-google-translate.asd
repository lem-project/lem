;;don't edit
(defsystem "lem-google-translate"
  :depends-on("lem/core"
              "translate-client")
  :class :package-inferred-system
  :components((:file "main"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")
