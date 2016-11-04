(in-package :lem)

(export '(popup-backtrace
          with-editor
          lem))

(defvar *running-p* nil)

(defun popup-backtrace (condition)
  (let ((buffer (get-buffer-create "*EDITOR ERROR*")))
    (display-buffer buffer)
    (set-buffer buffer)
    (buffer-erase)
    (with-open-stream (stream (make-buffer-output-stream buffer))
      (princ condition stream)
      (fresh-line stream)
      (uiop/image:print-backtrace
       :stream stream
       :count 100))))

(defun bailout (condition)
  (exit-editor
   (with-output-to-string (stream)
     (princ condition stream)
     (uiop/image:print-backtrace
      :stream stream
      :condition condition))))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind (#'(lambda (condition)
                           (handler-bind ((error #'bailout))
                             (popup-backtrace condition)))
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(add-hook 'find-file-hook
          (lambda ()
            (syntax-scan-window (current-window))))

(push #'syntax-scan-window *window-scroll-functions*)
(push #'syntax-scan-window *window-size-change-functions*)

(defun ask-revert-buffer ()
  (if (minibuf-y-or-n-p (format nil
                                "~A changed on disk; revert buffer?"
                                (buffer-filename)))
      (revert-buffer t)
      (update-changed-disk-date (current-buffer)))
  (redraw-display)
  (message nil))

(defvar *mainloop-waited-for-enough*)

(start-idle-timer "mainloop" 200 t
                  (lambda ()
                    (syntax-scan-window (current-window))
                    (redraw-display)
                    (setq *mainloop-waited-for-enough* t)))

(defun lem-mainloop ()
  (macrolet ((form (&body body)
                   `(cond (*debug-p*
                           (handler-bind ((error #'bailout)
                                          #+sbcl (sb-sys:interactive-interrupt #'bailout))
                             ,@body))
                          (t
                           ,@body))))
    (do-commandloop (:toplevel t)
      (with-error-handler ()
        (form
         (syntax-scan-lines (current-buffer)
                            (current-linum)
                            (1+ (current-linum)))
         (redraw-display)
         (let ((*mainloop-waited-for-enough* nil))
           (start-idle-timers)
           (let ((cmd (read-key-command)))
             (stop-idle-timers)
             (if (and *mainloop-waited-for-enough* (changed-disk-p (current-buffer)))
                 (ask-revert-buffer)
                 (progn
                   (message nil)
                   (handler-case
                       (handler-bind ((editor-condition
                                       (lambda (c)
                                         (declare (ignore c))
                                         (stop-record-key))))
                         (cmd-call cmd nil))
                     (editor-abort ()
                                   (buffer-mark-cancel (current-buffer))
                                   (message "Quit"))
                     (readonly ()
                               (message "Read Only"))
                     (editor-error (c)
                                   (message (editor-error-message c)))))))))))))

(let ((passed nil))
  (defun call-with-editor (function)
    (let ((*running-p* t))
      (unless passed
        (setq passed t)
        (display-init)
        (window-init)
        (minibuf-init)
        (run-hooks 'after-init-hook))
      (funcall function))))

(defmacro with-editor (() &body body)
  `(call-with-editor (lambda () ,@body)))

(defun lem-1 (args)
  (term-init)
  (let ((report (unwind-protect
                  (with-editor ()
                    (mapc 'find-file args)
                    (lem-mainloop))
                  (term-finallize))))
    (when report
      (format t "~&~a~%" report))))

(defun check-init ()
  (when *running-p*
    (error "~A is already running" *program-name*)))

(defun lem (&rest args)
  (check-init)
  (lem-1 args))
