;;don't edit
(DEFSYSTEM "lem-migemo" :CLASS :PACKAGE-INFERRED-SYSTEM :COMPONENTS
  ((:FILE "main")) :DEPENDS-ON (:CL-MIGEMO)
  :author "Masatoshi SANO <snmsts@gmail.com>"
  :license "MIT"
  :description "Japanese incremental search for the Lem editor")
(UNLESS (ASDF/SYSTEM:FIND-SYSTEM "cl-migemo" NIL)
  (UIOP/PACKAGE:SYMBOL-CALL "ROSWELL" "ROSWELL"
                            '("install" "snmsts/cl-migemo")))
