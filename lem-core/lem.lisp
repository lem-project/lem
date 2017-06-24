(in-package :lem)

(export '(*after-init-hook*
          with-editor
          lem))

(defvar *before-init-hook* '())

(defvar *after-init-hook* '())

(defvar *in-the-editor* nil)

(defvar *syntax-scan-window-recursive-p* nil)

(defun syntax-scan-window (window)
  (check-type window window)
  (when (and (enable-syntax-highlight-p (window-buffer window))
             (null *syntax-scan-window-recursive-p*))
    (let ((*syntax-scan-window-recursive-p* t))
      (syntax-scan-region
       (line-start (copy-point (window-view-point window) :temporary))
       (or (line-offset (copy-point (window-view-point window) :temporary)
                        (window-height window))
           (buffer-end-point (window-buffer window)))))))

(defun syntax-scan-buffer (buffer)
  (check-type buffer buffer)
  (syntax-scan-region
   (buffer-start-point buffer)
   (buffer-end-point buffer)))

(defun setup ()
  (start-idle-timer 100 t
                    (lambda ()
                      (syntax-scan-window (current-window))))
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
              (syntax-scan-region start end)))
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
                           (let ((report (with-catch-bailout
                                           (toplevel-command-loop
                                            initialize-function))))
                             (bt:interrupt-thread
                              main-thread
                              (lambda ()
                                (error 'exit-editor :value report)))))
                         :name "editor")))
    (handler-case (input-loop editor-thread)
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
               (call-with-screen
                (lambda ()
                  (unwind-protect
                       (progn
                         (setf *in-the-editor* t)
                         (unless passed
                           (setq passed t)
                           (window-init)
                           (minibuf-init)
                           (setup))
                         (funcall function))
                    (setf *in-the-editor* nil))))))))
      (when report
        (format t "~&~A~%" report)))))

(defmacro with-editor (() &body body)
  `(call-with-editor (lambda () ,@body)))

(defun parse-args (args)
  ;; stub
  (mapcar (lambda (file) `(find-file ,file)) args))

(let ((visited nil))
  (defun lem (&rest args)
    (setf args (parse-args args))
    (if *in-the-editor*
        (loop for exp in args do (eval exp))
        (progn
          (run-hooks *before-init-hook*)
          (with-editor ()
            (lem-internal
             (lambda ()
               (unless visited
                 (setf visited t)
                 (load-init-file) ;; need to idea for support '-q' '-u' on emacs
                 (run-hooks *after-init-hook*))
               (loop for exp in args do (eval exp)))))))))
