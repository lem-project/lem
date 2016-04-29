(in-package :lem)

(export '(lem))

(defvar *running-p* nil)

(defun load-init-file ()
  (flet ((test (path)
               (when (cl-fad:file-exists-p path)
                 (load path)
                 (message "Load file: ~a" path)
                 t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(defun popup-backtrace (condition)
  (info-popup (get-buffer-create "*Error*")
              #'(lambda (out)
                  (princ condition out)
                  (fresh-line out)
                  (uiop/image:print-backtrace
                   :stream out :count 100))))

(defmacro with-error-handler (() &body body)
  `(handler-case-bind (#'(lambda (condition)
                           (handler-bind ((error #'bailout))
                             (popup-backtrace condition)))
                       ,@body)
                      ((condition) (declare (ignore condition)))))

(defun lem-main ()
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
         (redraw-display)
         (let* ((key (read-key-sequence))
                (cmd (find-keybind key)))
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
               (message (editor-error-message c))))))))))

(let ((passed nil))
  (defun lem-init (args)
    (term-init)
    (setq *running-p* t)
    (when (not passed)
      (setq passed t)
      (display-init)
      (window-init)
      (minibuf-init)
      (with-error-handler ()
        (load-init-file)))
    (dolist (arg args)
      (find-file arg))))

(defun lem-finallize ()
  (term-finallize)
  (setq *running-p* nil))

(defun lem-1 (args)
  (let ((report
         (unwind-protect
           (progn
             (lem-init args)
             (lem-main))
           (lem-finallize))))
    (when report
      (format t "~&~a~%" report))))

(defun check-init ()
  (when *running-p*
    (error "~A is already running" *program-name*)))

(defun lem (&rest args)
  (check-init)
  (lem-1 args))

(defun bailout (condition)
  (exit-editor
   (with-output-to-string (stream)
     (princ condition stream)
     (uiop/image:print-backtrace
      :stream stream
      :condition condition))))
