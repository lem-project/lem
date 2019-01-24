;;don't edit
(defsystem "lem-google-translate"
  :depends-on("translate-client")
  :class :package-inferred-system
  :components((:file "main"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com"
  :description "use Google Translate in the Lem editor")
