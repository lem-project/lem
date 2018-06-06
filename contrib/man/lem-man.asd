;;don't edit
(DEFSYSTEM "lem-man" :CLASS :PACKAGE-INFERRED-SYSTEM :COMPONENTS
 ((:FILE "main")) :DEPENDS-ON (:SN.MAN) :PERFORM
 (COMPILE-OP :BEFORE (O C)
  (WHEN (FIND :ROS.INSTALLING *FEATURES*)
    (UIOP/PACKAGE:SYMBOL-CALL "ROSWELL" "ROSWELL"
                              '("install" "snmsts/sn.man")))))
