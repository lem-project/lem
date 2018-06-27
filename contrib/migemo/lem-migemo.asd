;;don't edit
(DEFSYSTEM "lem-migemo" :CLASS :PACKAGE-INFERRED-SYSTEM :COMPONENTS
 ((:FILE "main")) :DEPENDS-ON (:CL-MIGEMO))
(UNLESS (ASDF/SYSTEM:FIND-SYSTEM "cl-migemo" NIL)
  (UIOP/PACKAGE:SYMBOL-CALL "ROSWELL" "ROSWELL"
                            '("install" "snmsts/cl-migemo")))