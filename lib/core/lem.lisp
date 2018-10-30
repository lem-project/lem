(in-package :lem)

(export '(*before-init-hook*
          *after-init-hook*
          *splash-function*
          lem
          main))

(defvar *before-init-hook* '())
(defvar *after-init-hook* '())
(defvar *splash-function* nil)

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

(defun ask-revert-buffer ()
  (when (changed-disk-p (current-buffer))
    (cond ((eql (buffer-value (current-buffer) 'no-revert-buffer)
                (file-write-date (buffer-filename))))
          ((prompt-for-y-or-n-p (format nil "Revert buffer from file ~A" (buffer-filename)))
           (revert-buffer t))
          (t
           (setf (buffer-value (current-buffer) 'no-revert-buffer)
                 (file-write-date (buffer-filename)))))))

(let ((once nil))
  (defun setup ()
    (setup-windows)
    (setup-minibuffer)
    (unless once
      (setf once t)
      (start-idle-timer 100 t
                        (lambda ()
                          (syntax-scan-window (current-window)))
                        nil "syntax-scan")
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
      (add-hook (variable-value 'before-save-hook :global)
                (lambda (buffer)
                  (scan-file-property-list buffer)))
      (start-idle-timer 100 t
                        'ask-revert-buffer))))

(defun teardown ()
  (teardown-windows)
  (teardown-minibuffer))

(defstruct command-line-arguments
  args
  (no-init-file nil))

(defun parse-args (args)
  (let ((parsed-args
          (make-command-line-arguments))
        (file-count 0))
    (setf (command-line-arguments-args parsed-args)
          `(,@(loop :while args
                    :for arg := (pop args)
                    :when (cond ((member arg '("-q" "--no-init-file") :test #'equal)
                                 (setf (command-line-arguments-no-init-file parsed-args)
                                       t)
                                 nil)
                                ((equal arg "--eval")
                                 `(eval ,(read-from-string (pop args))))
                                ((equal arg "--kill")
                                 `(uiop:quit))
                                ((member arg '("-v" "--version") :test #'equal)
                                 (format t "~a~%" (lem-version))
                                 (uiop:quit)
                                 nil)
                                ((or (stringp arg) (pathnamep arg))
                                 (incf file-count)
                                 `(find-file ,(merge-pathnames arg (uiop:getcwd))))
                                (t
                                 arg))
                    :collect it)
            ,@(and (zerop file-count)
                   *splash-function*
                   `((funcall ,*splash-function*)))))
    parsed-args))

(defun apply-args (args)
  (mapc #'eval (command-line-arguments-args args)))

(defvar *original-home* (user-homedir-pathname))

(defun init-quicklisp (path)
  (flet ((replace-homedir (x)
           (let ((subpath (uiop:subpathp x *original-home*)))
             (if subpath
                 (merge-pathnames subpath (user-homedir-pathname))
                 x))))
    (setf ql:*quicklisp-home* (ensure-directories-exist path)
          ql:*local-project-directories* (mapcar #'replace-homedir ql:*local-project-directories*)
          asdf:*central-registry* (mapcar #'replace-homedir asdf:*central-registry*)
          asdf:*user-cache* (replace-homedir asdf:*user-cache*)))
  (asdf:initialize-source-registry)
  (asdf:clear-output-translations)
  (unless (uiop:directory-exists-p (ql:qmerge (format nil "dists/~A/" (pathname-name ql:*initial-dist-url*))))
    (ql-dist:install-dist ql:*initial-dist-url* :prompt nil)))

(defun lem-home ()
  (or (uiop:getenv "LEM_HOME")
      (merge-pathnames ".lem/" (user-homedir-pathname))))

(let ((once nil))
  (defun init (args)
    (unless once
      (unless (equal (funcall 'user-homedir-pathname) ;; funcall for sbcl optimization
                     *original-home*)
        (init-quicklisp (merge-pathnames "quicklisp/" (lem-home))))
      (setf once t)
      (uiop:symbol-call :lem :load-site-init)
      (run-hooks *before-init-hook*)
      (unless (command-line-arguments-no-init-file args)
        (load-init-file))
      (run-hooks *after-init-hook*))
    (apply-args args)))

(defun run-editor-thread (initialize args finalize)
  (bt:make-thread
   (lambda ()
     (when initialize (funcall initialize))
     (unwind-protect
         (let (#+lispworks (lw:*default-character-element-type* 'character)
               (*standard-output* (make-editor-output-stream)))
           (setf *in-the-editor* t)
           (setup)
           (let ((report (toplevel-command-loop (lambda () (init args)))))
             (when finalize (funcall finalize report))
             (teardown)))
       (setf *in-the-editor* nil)))
   :name "editor"))

(defun lem (&rest args)
  (setf args (parse-args args))
  (if *in-the-editor*
      (apply-args args)
      (invoke-frontend
       (lambda (&optional initialize finalize)
         (run-editor-thread initialize args finalize)))))

(defun main (&optional (args (uiop:command-line-arguments)))
  (apply #'lem args))
