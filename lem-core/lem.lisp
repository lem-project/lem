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
      (exit-editor (c) (return-from lem-internal (exit-editor-value c))))))

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

(defstruct command-line-arguments
  args
  (no-init-file nil))

(defun parse-args (args)
  (let ((parsed-args
          (make-command-line-arguments)))
    (loop :with args2 := '()
          :while args
          :for arg := (pop args)
          :do (cond ((member arg '("-q" "--no-init-file") :test #'equal)
                     (setf (command-line-arguments-no-init-file parsed-args)
                           t))
                    ((or (stringp arg) (pathnamep arg))
                     (push `(find-file ,(merge-pathnames arg (uiop:getcwd)))
                           args2))
                    (t
                     (push arg args2)))
          :finally (setf (command-line-arguments-args parsed-args)
                         (nreverse args2)))
    parsed-args))

(defun apply-args (args)
  (mapc #'eval (command-line-arguments-args args)))

(let ((visited nil))
  (defun lem (&rest args)
    (setf args (parse-args args))
    (if *in-the-editor*
        (apply-args args)
        (progn
          (run-hooks *before-init-hook*)
          (with-editor ()
            (lem-internal
             (lambda ()
               (unless visited
                 (setf visited t)
                 (unless (command-line-arguments-no-init-file args)
                   (load-init-file))
                 (run-hooks *after-init-hook*))
               (apply-args args))))))))
