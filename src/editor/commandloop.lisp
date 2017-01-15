(in-package :lem)

(export '(*pre-command-hook*
          *post-command-hook*
          *exit-editor-hook*
          interactive-p
          add-continue-flag
          continue-flag
          pop-up-backtrace))

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
  (let ((buffer (get-buffer-create "*EDITOR ERROR*")))
    (erase-buffer buffer)
    (display-buffer buffer)
    (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer)))
      (princ condition stream)
      (fresh-line stream)
      (uiop/image:print-backtrace
       :stream stream
       :count 100))))

(defvar *interactive-p* nil)
(defun interactive-p () *interactive-p*)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *continue-command-flags*
  (list :next-line :kill :undo :yank))

(defun add-continue-flag (keyword)
  (pushnew keyword *continue-command-flags*))

(defun %make-flags ()
  (mapcar #'(lambda (sym)
              (cons sym nil))
          *continue-command-flags*))

(defun continue-flag (flag)
  (prog1 (cdr (assoc flag *last-flags*))
    (push (cons flag t) *last-flags*)
    (push (cons flag t) *curr-flags*)))

(defun cmd-call (cmd arg)
  (run-hooks *pre-command-hook*)
  (prog1 (funcall cmd arg)
    (buffer-undo-boundary)
    (run-hooks *post-command-hook*)))

(defun do-commandloop-function (function)
  (do ((*curr-flags* (%make-flags) (%make-flags))
       (*last-flags* (%make-flags) *curr-flags*))
      (nil)
    (let ((*interactive-p* t))
      (funcall function))))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind ((lambda (condition)
                         (handler-bind ((error #'bailout))
                           (pop-up-backtrace condition)))
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(defmacro do-commandloop ((&key toplevel) &body body)
  `(if ,toplevel
       (catch +exit-tag+
         (do-commandloop-function
           (lambda ()
             (with-error-handler ()
               ,@body))))
       (do-commandloop-function (lambda () ,@body))))

(defun command-loop (toplevel)
  (do-commandloop (:toplevel toplevel)
    (when (= 0 (event-queue-length)) (redraw-display))
    (handler-case
        (handler-bind ((editor-condition
                        (lambda (c)
                          (declare (ignore c))
                          (stop-record-key))))
          (let ((cmd (progn
                       (start-idle-timers)
                       (prog1 (read-key-command)
                         (stop-idle-timers)))))
            (unless (minibuffer-window-active-p) (message nil))
            (cmd-call cmd nil)))
      (editor-condition (c)
                        (message "~A" c)))))

(defun exit-editor (&optional report)
  (run-hooks *exit-editor-hook*)
  (throw +exit-tag+ report))
