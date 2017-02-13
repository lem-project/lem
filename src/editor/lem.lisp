(in-package :lem)

(export '(*after-init-hook*
          with-editor
          lem))

(defvar *after-init-hook* '())

(defvar *in-the-editor* nil)

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
  (syntax-scan-range (buffers-start buffer) (buffers-end buffer)))

(defun setup ()
  (start-idle-timer "mainloop" 100 t
                    (lambda ()
                      (syntax-scan-window (current-window))
                      (redraw-display)))
  (add-hook *window-scroll-functions*
            (lambda (window)
              (syntax-scan-window window)))
  (add-hook *window-size-change-functions*
            (lambda (window)
              (syntax-scan-window window)))
  (add-hook *window-show-buffer-functions*
            (lambda (window)
              (syntax-scan-window window)))
  (add-hook (variable-value 'after-change-functions :global)
            (lambda (start end old-len)
              (declare (ignore old-len))
              (syntax-scan-range start end)))
  (add-hook *find-file-hook*
            (lambda (buffer)
              (prepare-auto-mode buffer)
              (scan-file-property-list buffer)
              (syntax-scan-buffer buffer))
            5000)
  (add-hook *before-save-hook*
            (lambda (buffer)
              (scan-file-property-list buffer))))

(defun lem-internal (initialize-function)
  (let* ((main-thread (bt:current-thread))
         (editor-thread (bt:make-thread
                         (lambda ()
                           (let ((*in-the-editor* t))
                             (let ((report (with-catch-bailout
                                             (toplevel-command-loop
                                              initialize-function))))
                               (bt:interrupt-thread
                                main-thread
                                (lambda ()
                                  (error 'exit-editor :value report))))))
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
      (exit-editor (c) (return-from lem-internal (exit-editor-value c)))
      #+sbcl
      (sb-sys:interactive-interrupt (c)
                                    (declare (ignore c))
                                    (bt:destroy-thread editor-thread)))))

(let ((passed nil))
  (defun call-with-editor (function)
    (let ((report
           (with-catch-bailout
             (handler-bind ((error #'bailout)
                            #+sbcl (sb-sys:interactive-interrupt #'bailout))
               (unwind-protect (let ((*in-the-editor* t))
                                 (unless passed
                                   (setq passed t)
                                   (display-init)
                                   (window-init)
                                   (minibuf-init)
                                   (setup))
                                 (funcall function))
                 (display-finalize))))))
      (when report
        (format t "~&~A~%" report)))))

(defmacro with-editor (() &body body)
  `(call-with-editor (lambda () ,@body)))

(defun lem (&rest args)
  (if *in-the-editor*
      (mapc 'find-file args)
      (with-editor ()
        (lem-internal
         (lambda ()
           (run-hooks *after-init-hook*)
           (mapc 'find-file args))))))
