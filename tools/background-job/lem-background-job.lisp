(cl:defpackage :lem-background-job
  (:use :cl :lem)
  (:export :funcall-background-job-with-display-buffer))
(in-package :lem-background-job)

(defstruct job
  name
  timer
  thread)

(defun funcall-background-job-with-display-buffer
    (buffer fn args &key (name nil namep) (input-string "") (timer-interval 100))
  (let* (results
         output-string
         error-p
         (body-fn
          (lambda ()
            (with-output-to-string (output)
              (with-input-from-string (input input-string)
                (let* ((io (make-two-way-stream input output))
                       (*terminal-io* io)
                       (*standard-output* io)
                       (*standard-input* io)
                       (*error-output* io)
                       (*query-io* io)
                       (*debug-io* io)
                       (*trace-output* io))
                  (handler-case
                      (progn
                        (setq results (multiple-value-list (funcall fn args)))
                        (setq output-string (get-output-stream-string output))
                        (setq error-p nil))
                    (error (c)
                           (setq error-p t)
                           (setq output-string
                                 (with-output-to-string (stream)
                                   (uiop:print-backtrace :stream stream :condition c))))))))))
         (thread (if namep
                     (bt:make-thread body-fn :name name)
                     (bt:make-thread body-fn)))
         timer)
    (setq timer
          (start-timer name
                       timer-interval
                       t
                       (lambda ()
                         (unless (bt:thread-alive-p thread)
                           (display-buffer buffer)
                           (let ((output (make-buffer-output-stream buffer (point-max buffer))))
                             (fresh-line output)
                             (princ #\Page output)
                             (terpri output)
                             (princ output-string output)
                             (unless error-p
                               (dolist (r results)
                                 (pprint r output)))
                             (stop-timer timer))))
                       nil
                       (lambda (condition)
                         (popup-backtrace condition)
                         (stop-timer timer))))
    (make-job :name name :timer timer :thread thread)))
