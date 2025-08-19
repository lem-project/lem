(defpackage :lem/commands/sprof
  (:use :cl
        :lem-core))
(in-package :lem/commands/sprof)

(define-command lem-sprof-start () ()
  (sb-sprof:start-profiling))

(define-command lem-sprof-report () ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (let ((file (format nil
                        "lem-profile-report-~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
                        year
                        month
                        day
                        hour
                        min
                        sec)))
      (with-open-file (stream file 
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (sb-sprof:report :stream stream))
      (sb-sprof:stop-profiling)
      (sb-sprof:reset))))
