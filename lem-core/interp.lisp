(in-package :lem)

(export '(*pre-command-hook*
          *post-command-hook*
          *exit-editor-hook*
          interactive-p
          continue-flag
          call-command
          pop-up-backtrace
          call-background-job))

(defvar *pre-command-hook* '())
(defvar *post-command-hook* '())
(defvar *exit-editor-hook* '())

(defvar +exit-tag+ (gensym "EXIT"))
(defvar +bailout-tag+ (make-symbol "BAILOUT"))

(defmacro with-catch-bailout (&body body)
  `(catch +bailout-tag+
     ,@body))

(defun bailout (condition)
  (throw +bailout-tag+
    (with-output-to-string (stream)
      (princ condition stream)
      (uiop/image:print-backtrace
       :stream stream
       :condition condition))))

(defun pop-up-backtrace (condition)
  (let ((buffer (make-buffer "*EDITOR ERROR*")))
    (erase-buffer buffer)
    (display-buffer buffer)
    (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer)))
      (princ condition stream)
      (fresh-line stream)
      (uiop/image:print-backtrace
       :stream stream
       :count 100))))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind (#'bailout
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(defvar *interactive-p* nil)
(defun interactive-p () *interactive-p*)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defun continue-flag (flag)
  (prog1 (cdr (assoc flag *last-flags*))
    (push (cons flag t) *last-flags*)
    (push (cons flag t) *curr-flags*)))

(defun call-command (command-name arg)
  (run-hooks *pre-command-hook*)
  (prog1 (let ((cmd (get-command command-name)))
           (if cmd
               (funcall cmd arg)
               (editor-error "~A: command not found" command-name)))
    (buffer-undo-boundary)
    (run-hooks *post-command-hook*)))

(defmacro do-command-loop ((&key interactive) &body body)
  (alexandria:once-only (interactive)
    `(loop :for *last-flags* := nil :then *curr-flags*
           :for *curr-flags* := nil
           :do (let ((*interactive-p* ,interactive)) ,@body))))

(defun command-loop ()
  (do-command-loop (:interactive t)
    (with-error-handler ()
      (when (= 0 (event-queue-length))
        (without-interrupts
          (handler-bind ((error #'bailout))
            (redraw-display))))
      (handler-case
          (handler-bind ((editor-abort
                           (lambda (c)
                             (declare (ignore c))
                             (buffer-mark-cancel (current-buffer))))
                         (editor-condition
                           (lambda (c)
                             (declare (ignore c))
                             (stop-record-key))))
            (let ((cmd (progn
                         (start-idle-timers)
                         (prog1 (read-command)
                           (stop-idle-timers)))))
              (unless (minibuffer-window-active-p) (message nil))
              (clear-balloon-message)
              (call-command cmd nil)))
        (editor-condition (c)
          (message "~A" c))))))

(defun toplevel-command-loop (initialize-function)
  (with-catch-bailout
    (catch +exit-tag+
      (with-error-handler ()
        (funcall initialize-function))
      (let ((*standard-output* (make-editor-output-stream)))
        (command-loop)))))

(defun exit-editor (&optional report)
  (run-hooks *exit-editor-hook*)
  (throw +exit-tag+ report))

(defun call-background-job (function cont)
  (bt:make-thread
   (lambda ()
     (let ((error-text))
       (handler-case
           (handler-bind ((error (lambda (c)
                                   (setf error-text
                                         (with-output-to-string (stream)
                                           (princ c stream)
                                           (fresh-line stream)
                                           (uiop:print-backtrace
                                            :stream stream
                                            :count 100))))))
             (let ((result (funcall function)))
               (send-event (lambda () (funcall cont result)))))
         (error ()
           (send-event (lambda ()
                         (let ((buffer (make-buffer "*BACKGROUND JOB ERROR*")))
                           (erase-buffer buffer)
                           (insert-string (buffer-point buffer)
                                          error-text)
                           (display-buffer buffer)
                           (buffer-start (buffer-point buffer)))))))))))
