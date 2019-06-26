;;don't edit
(defsystem "lem-man"
  :class :package-inferred-system
  :components((:file "main"))
  :depends-on("lem"
              :sn.man))
(UNLESS (ASDF/SYSTEM:FIND-SYSTEM "sn.man" NIL)
  (UIOP/PACKAGE:SYMBOL-CALL "ROSWELL" "ROSWELL" '("install" "snmsts/sn.man")))