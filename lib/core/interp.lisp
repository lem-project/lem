(in-package :lem)

(export '(*this-command*
          *universal-argument*
          *pre-command-hook*
          *post-command-hook*
          *exit-editor-hook*
          interactive-p
          continue-flag
          call-command
          pop-up-backtrace
          call-background-job))

(defvar *this-command* nil)
(defvar *universal-argument*)
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
  (let ((o (with-output-to-string (stream)
             (princ condition stream)
             (fresh-line stream)
             (uiop/image:print-backtrace
              :stream stream
              :count 100))))
    (funcall 'pop-up-typeout-window
             (make-buffer "*EDITOR ERROR*")
             (lambda (stream)
               (format stream "~A" o))
             :focus t
             :erase t)))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind ((lambda (condition)
                         (handler-bind ((error #'bailout))
                           (pop-up-backtrace condition)
                           (redraw-display)))
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

(defun call-command (*this-command* *universal-argument*)
  (run-hooks *pre-command-hook*)
  (prog1 (let ((cmd (get-command *this-command*)))
           (if cmd
               (funcall cmd *universal-argument*)
               (editor-error "~A: command not found" *this-command*)))
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
            (redraw-display*))))
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
              (call-command cmd nil)))
        (editor-condition (c)
          (message "~A" c))))
    (unless (eq (window-buffer (current-window))
                (current-buffer))
      (setf (current-buffer) (window-buffer (current-window))))))

(defun toplevel-command-loop (initialize-function)
  (with-catch-bailout
    (catch +exit-tag+
      (with-error-handler ()
        (funcall initialize-function))
      (let ((*standard-output* (make-editor-output-stream)))
        (command-loop)))))

(defun exit-editor (&optional report)
  (run-hooks *exit-editor-hook*)
  (mapc #'disable-minor-mode *global-minor-mode-list*)
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
