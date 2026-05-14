(defpackage :lem/commands/sprof
  (:use :cl
        :lem-core))
(in-package :lem/commands/sprof)

(defun start-profiling-in-mode (mode)
  "Reset any prior profile and start a new one in MODE (`:alloc' or `:cpu')."
  (sb-sprof:stop-profiling)
  (sb-sprof:reset)
  (sb-sprof:start-profiling :mode mode)
  (message "Profiling started (~(~A~) mode)" mode))

(define-command lem-sprof-start () ()
  "Start statistical profiling in CPU-sampling mode.

This is the default starting point.  Use `lem-sprof-start-alloc' when
you suspect allocation pressure rather than a CPU-bound loop.  Any
in-progress profile is stopped and reset first, so calling this twice
is safe."
  (start-profiling-in-mode :cpu))

(define-command lem-sprof-start-alloc () ()
  "Start statistical profiling in allocation-sampling mode.

Useful when the suspected bottleneck is heap allocation rather than CPU
work; otherwise `lem-sprof-start' (CPU mode) is the default.  Any
in-progress profile is stopped and reset first."
  (start-profiling-in-mode :alloc))

(define-command lem-sprof-report () ()
  "Stop profiling, write the report to /tmp/, and reset.
Reports the absolute path of the generated file via `message'."
  (sb-sprof:stop-profiling)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (let ((file (format nil
                        "/tmp/lem-profile-report-~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D.txt"
                        year month day hour min sec)))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (sb-sprof:report :stream stream))
      (sb-sprof:reset)
      (message "Profile written to ~A" file))))
