(in-package :lem)

(export '(*universal-argument*
          *exit-editor-hook*
          interactive-p
          continue-flag
          pop-up-backtrace
          call-background-job))

(defvar *universal-argument*)
(defvar *exit-editor-hook* '())

(defparameter +exit-tag+ (make-symbol "EXIT"))
(defparameter +bailout-tag+ (make-symbol "BAILOUT"))

(defmacro with-catch-bailout (&body body)
  `(catch +bailout-tag+
     ,@body))

(defun bailout (condition)
  (throw +bailout-tag+
    (with-output-to-string (stream)
      (princ condition stream)
      (uiop:print-backtrace
       :stream stream
       :condition condition))))

(defun pop-up-backtrace (condition)
  (let ((o (with-output-to-string (stream)
             (princ condition stream)
             (fresh-line stream)
             (uiop:print-backtrace
              :stream stream
              :count 100))))
    (funcall 'pop-up-typeout-window
             (make-buffer "*EDITOR ERROR*")
             (lambda (stream)
               (format stream "~A" o))
             :focus t
             :erase t)))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error
                        (lambda (condition)
                          (handler-bind ((error #'bailout))
                            (pop-up-backtrace condition)
                            (redraw-display)))))
         ,@body)
     (error ())))

(defvar *interactive-p* nil)
(defun interactive-p () *interactive-p*)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defun continue-flag (flag)
  (prog1 (cdr (assoc flag *last-flags*))
    (push (cons flag t) *last-flags*)
    (push (cons flag t) *curr-flags*)))

(defmacro do-command-loop ((&key interactive) &body body)
  (alexandria:once-only (interactive)
    `(loop :for *last-flags* := nil :then *curr-flags*
           :for *curr-flags* := nil
           :do (let ((*interactive-p* ,interactive)) ,@body))))

(defun fix-current-buffer-if-broken ()
  (unless (eq (window-buffer (current-window))
              (current-buffer))
    (setf (current-buffer) (window-buffer (current-window)))))

(defvar *editor-condition-handlers* (make-hash-table))

(defun register-command-loop-condition-handler (condition-name handler)
  (pushnew handler (gethash condition-name *editor-condition-handlers*)))

(defun command-loop-body ()
  (flet ((redraw ()
           (when (= 0 (event-queue-length))
             (without-interrupts
               (handler-bind ((error #'bailout))
                 (redraw-display)))))

         (read-command-and-call ()
           (let ((cmd (progn
                        (start-idle-timers)
                        (prog1 (read-command)
                          (stop-idle-timers)))))
             (message nil)
             (call-command cmd nil)))

         (editor-abort-handler (c)
           (declare (ignore c))
           (buffer-mark-cancel (current-buffer)))

         (editor-condition-handler (c)
           (declare (ignore c))
           (stop-record-key))

         (editor-condition-handler! (c)
           (dolist (function (gethash (type-of c) *editor-condition-handlers*))
             (funcall function c))))

    (redraw)

    (handler-case
        (handler-bind ((editor-abort
                         #'editor-abort-handler)
                       (editor-condition
                         #'editor-condition-handler))
          (handler-bind ((editor-condition
                           #'editor-condition-handler!))
            (read-command-and-call)))
      (editor-condition (c)
        (restart-case (error c)
          (lem-restart:message ()
            (typecase c
              (editor-abort
               (let ((message (princ-to-string c)))
                 (unless (string= "" message)
                   (message "~A" message))))
              (otherwise
               (message "~A" c))))
          (lem-restart:call-function (fn)
            (funcall fn)))))))

(defvar *toplevel-command-loop-p* t)

(defun toplevel-command-loop-p ()
  *toplevel-command-loop-p*)

(defun command-loop ()
  (do-command-loop (:interactive t)
    (if (toplevel-command-loop-p)
        (handler-bind ((<executing-command> #'handle-signal))
          (with-error-handler ()
            (let ((*toplevel-command-loop-p* nil))
              (handler-bind ((editor-condition
                               (lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'lem-restart:message))))
                (command-loop-body)))))
        (command-loop-body))
    (fix-current-buffer-if-broken)))

(defun toplevel-command-loop (initialize-function)
  (with-catch-bailout
    (catch +exit-tag+
      (with-error-handler ()
        (funcall initialize-function))
      (with-editor-stream ()
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
