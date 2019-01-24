;;don't edit
(defsystem "lem-encodings-table"
  :depends-on (:ICONV)
  :class :package-inferred-system
  :components ((:FILE "8bit") (:FILE "euc") (:FILE "sjis") (:FILE "main"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com"
  :license "MIT"
  :description "character encoding tables for the Lem editor")
