;;don't edit
(defsystem "lem-encodings"
  :depends-on (:LEM-BASE)
  :class :package-inferred-system
  :components ((:FILE "table") (:FILE "8bit") (:FILE "gb2312") (:FILE "euc-jp")
 (:FILE "cp932") (:FILE "iso-8859-1") (:FILE "utf-8") (:FILE "utf-16"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com"
  :license "MIT"
  :description "character encodings for the Lem editor")
