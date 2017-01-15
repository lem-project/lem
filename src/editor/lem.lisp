(in-package :lem)

(export '(*debug-p*
          *after-init-hook*
          pop-up-backtrace
          with-editor
          lem))

(defvar *after-init-hook* '())

(defvar *debug-p* nil)
(defvar *in-the-editor* nil)

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

(defun error-report (condition)
  (with-output-to-string (stream)
    (princ condition stream)
    (uiop/image:print-backtrace
     :stream stream
     :condition condition)))

(defun bailout (condition)
  (throw 'toplevel
    (error-report condition)))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind ((lambda (condition)
                         (if *debug-p*
                             (bailout condition)
                             (handler-bind ((error #'bailout))
                               (pop-up-backtrace condition))))
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(defvar *syntax-scan-window-recursive-p* nil)

(defun syntax-scan-window (window)
  (check-type window window)
  (when (and (enable-syntax-highlight-p (window-buffer window))
             (null *syntax-scan-window-recursive-p*))
    (let ((*syntax-scan-window-recursive-p* t))
      (window-see window)
      (syntax-scan-range (line-start (copy-point (window-view-point window) :temporary))
                         (or (line-offset (copy-point (window-view-point window) :temporary)
                                          (window-height window))
                             (buffers-end (window-buffer window)))))))

(defun syntax-scan-buffer (buffer)
  (check-type buffer buffer)
  (when (enable-syntax-highlight-p buffer)
    (syntax-scan-range (buffers-start buffer) (buffers-end buffer))))

(defun syntax-scan-current-view (&optional (window (current-window)))
  (cond
    ((get-bvar 'already-visited :buffer (window-buffer window))
     (syntax-scan-window window))
    (t
     (setf (get-bvar 'already-visited :buffer (window-buffer window)) t)
     (syntax-scan-buffer (window-buffer window)))))

(defun syntax-scan-point (start end old-len)
  (line-start start)
  (if (zerop old-len)
      (syntax-scan-range start end)
      (syntax-scan-range (line-start start) (line-end end))))

(defun setup ()
  (start-idle-timer "mainloop" 200 t
                    (lambda ()
                      (redraw-display)))
  (start-idle-timer "lazy-syntax-scan" 500 t
                    (lambda ()
                      (syntax-scan-current-view (current-window))
                      (redraw-display)))
  (add-hook *window-scroll-functions*
            (lambda (window)
              (syntax-scan-current-view window)))
  (add-hook *window-size-change-functions*
            (lambda (window)
              (syntax-scan-current-view window)))
  (add-hook *window-show-buffer-functions*
            (lambda (window)
              (syntax-scan-window window)))
  (add-hook *after-change-functions*
            'syntax-scan-point)
  (add-hook *find-file-hook*
            (lambda (buffer)
              (prepare-auto-mode buffer)
              (scan-file-property-list buffer)
              (syntax-scan-buffer buffer)))
  (add-hook *before-save-hook*
            (lambda (buffer)
              (scan-file-property-list buffer))))

(defun read-key-command-with-idle-timers ()
  (start-idle-timers)
  (prog1 (read-key-command)
    (stop-idle-timers)))

(defun toplevel-editor-loop ()
  (catch 'toplevel
    (do-commandloop (:toplevel t)
      (with-error-handler ()
        (when (= 0 (event-queue-length)) (redraw-display))
        (handler-case
            (handler-bind ((editor-condition
                            (lambda (c)
                              (declare (ignore c))
                              (stop-record-key))))
              (let ((cmd (read-key-command-with-idle-timers)))
                (message nil)
                (cmd-call cmd nil)))
          (editor-abort ()
                        (buffer-mark-cancel (current-buffer))
                        (message "Quit"))
          (editor-condition (c)
                            (message "~A" c)))))
    nil))

(defun lem-internal ()
  (let* ((main-thread (bt:current-thread))
         (editor-thread (bt:make-thread
                         (lambda ()
                           (let* ((*in-the-editor* t)
                                  (result (toplevel-editor-loop)))
                             (bt:interrupt-thread main-thread
                                                  (lambda ()
                                                    (error 'exit-editor
                                                           :value result)))))
                         :name "editor")))
    (handler-case
        (loop
          (unless (bt:thread-alive-p editor-thread) (return))
          (let ((code (charms/ll:getch)))
            (cond ((= code -1))
                  ((= code 410)
                   (send-resize-screen-event charms/ll:*cols* charms/ll:*lines*))
                  ((= code (char-code C-\]))
                   (bt:interrupt-thread editor-thread
                                        (lambda ()
                                          (error 'editor-interrupt))))
                  (t
                   (send-event
                    (let ((nbytes (utf8-bytes code)))
                      (if (= nbytes 1)
                          (code-char code)
                          (let ((vec (make-array nbytes :element-type '(unsigned-byte 8))))
                            (setf (aref vec 0) code)
                            (loop :for i :from 1 :below nbytes
                                  :do (setf (aref vec i) (charms/ll:getch)))
                            (schar (babel:octets-to-string vec) 0)))))))))
      (exit-editor (c)
                   (return-from lem-internal (exit-editor-value c))))))

(let ((passed nil))
  (defun call-with-editor (function)
    (handler-bind ((error #'error-report)
                   #+sbcl (sb-sys:interactive-interrupt #'error-report))
      (unwind-protect (let ((*in-the-editor* t))
                        (unless passed
                          (setq passed t)
                          (display-init)
                          (window-init)
                          (minibuf-init)
                          (setup)
                          (run-hooks *after-init-hook*))
                        (funcall function))
        (display-finalize)))))

(defmacro with-editor (() &body body)
  `(call-with-editor (lambda () ,@body)))

(defun lem (&rest args)
  (if *in-the-editor*
      (mapc 'find-file args)
      (let ((report (with-editor ()
                      (mapc 'find-file args)
                      (lem-internal))))
        (when report
          (format t "~&~A~%" report)))))
