;;don't edit
(DEFSYSTEM "lem-man" :CLASS :PACKAGE-INFERRED-SYSTEM :COMPONENTS
  ((:FILE "main"))
  :depends-on (:sn.man)
  :author "Masatoshi SANO <snmsts@gmail.com>"
  :license "MIT"
  :description "show man for lem.")

(UNLESS (ASDF:FIND-SYSTEM "sn.man" NIL)
  (UIOP/PACKAGE:SYMBOL-CALL "ROSWELL" "ROSWELL"
                            '("install" "snmsts/sn.man")))
