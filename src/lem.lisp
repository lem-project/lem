(in-package :lem-core)

(define-condition command-line-arguments-error (simple-error) ())

(defvar *set-location-hook* '((push-buffer-point . 0)))
(defvar *before-init-hook* '())
(defvar *after-init-hook* '())
(defvar *splash-function* nil)

(defvar *in-the-editor* nil)

(defvar *syntax-scan-window-recursive-p* nil)

(defvar *help* "Usage: lem [ OPTION-OR-FILENAME ] ...
Options:
        -q, --without-init-file        do not load ~/.lem/init.lisp
        --debug                   enable debugger
        --log-filename FILENAME   file name of the log file
        -i, --interface INTERFACE interface to use, either sdl2 or ncurses
        -v, --version             print the version number and exit
        -h, --help                display this help and exit"
"Help output for cli")

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

(defun setup-first-frame ()
  (let ((frame (make-frame nil)))
    (map-frame (implementation) frame)
    (setup-frame frame (primordial-buffer))))

(defclass lem-timer-manager (timer-manager) ())
(defmethod send-timer-notification ((lem-timer-manager timer-manager) continue)
  (send-event (lambda ()
                (funcall continue)
                (redraw-display))))

(let ((once nil))
  (defun setup ()
    (setup-first-frame)
    (unless once
      (setf once t)
      (start-timer (make-idle-timer (lambda ()
                                      (syntax-scan-window (current-window)))
                                    :name "syntax-scan")
                   100 :repeat t)
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
                  (process-file buffer)
                  (syntax-scan-buffer buffer))
                5000)
      (add-hook (variable-value 'before-save-hook :global)
                (lambda (buffer)
                  (process-file buffer)))
      (add-hook *input-hook*
                (lambda (event)
                  (push event *this-command-keys*))))))

(defun teardown ()
  (teardown-frames))

(defstruct command-line-arguments
  args
  (help nil)
  (debug nil)
  (version nil)
  (without-init-file nil)
  (log-filename nil)
  (interface nil)
  (filenames '()))

(defun use-spash-screen-p (args)
  (null (command-line-arguments-filenames args)))

(defun command-line-arguments-error (fmt &rest args)
  (error 'command-line-arguments-error :format-control fmt :format-arguments args))

(defun parse-args (args)
  (let ((help nil)
        (debug nil)
        (version nil)
        (without-init-file nil)
        (log-filename nil)
        (interface nil)
        (filenames '()))
    (loop :while args
          :for arg := (pop args)
          :do (cond ((member arg '("-h" "--help") :test #'equal)
                     (setf help t))
                    ;; TODO: without-intt-fileの方がよさそう
                    ((member arg '("-q" "--without-init-file") :test #'equal)
                     (setf without-init-file t))
                    ((equal arg "--debug")
                     (setf debug t))
                    ((equal arg "--log-filename")
                     (let ((filename (pop args)))
                       (unless filename
                         (command-line-arguments-error "Please, specify a filename to log to."))
                       (setf log-filename filename)))
                    ((member arg '("-i" "--interface") :test #'equal)
                     (let ((arg (pop args)))
                       (if arg
                           (setf interface
                                 (alexandria:make-keyword (string-upcase arg)))
                           (command-line-arguments-error "Please specify an interface to use."))))
                    ((member arg '("-v" "--version") :test #'equal)
                     (setf version t))
                    ((or (stringp arg) (pathnamep arg))
                     (push arg filenames))
                    (t
                     (command-line-arguments-error "unexpected arg: ~A" arg))))
    (make-command-line-arguments :help help
                                 :debug debug
                                 :version version
                                 :without-init-file without-init-file
                                 :log-filename log-filename
                                 :interface interface
                                 :filenames (nreverse filenames))))

(defun apply-args (args)
  (if (and (use-spash-screen-p args)
           *splash-function*)
      (funcall *splash-function*)
      (loop :for filename :in (command-line-arguments-filenames args)
            :do (uiop:symbol-call :lem :find-file (merge-pathnames filename (uiop:getcwd))))))

(defun load-init-file ()
  (flet ((maybe-load (path)
           (when (probe-file path)
             (load path)
             (message "Load file: ~a" path)
             t)))
    (let ((home (user-homedir-pathname))
          (current-dir (probe-file "."))
          (*package* (find-package :lem-user)))
      (or (maybe-load (merge-pathnames "init.lisp" (lem-home)))
          (maybe-load (merge-pathnames ".lemrc" home)))
      (unless (uiop:pathname-equal current-dir (user-homedir-pathname))
        (maybe-load (merge-pathnames ".lemrc" current-dir))))))

(defun initialize-source-registry ()
  (asdf:initialize-source-registry
   `(:source-registry
     :inherit-configuration
     (:also-exclude ".qlot")
     (:tree ,(asdf:system-source-directory :lem)))))

(defun init-at-build-time ()
  "This function is called when an lem executable file is built.
If a file named $HOME/.lem/build-init.lisp exists, it is loaded.
The difference is that init.lisp loading is called when the editor is started,
while build-init.lisp is called when the binary file is created.
See scripts/build-ncurses.lisp or scripts/build-sdl2.lisp"
  (initialize-source-registry)
  (let ((file (merge-pathnames "build-init.lisp" (lem-home))))
    (when (uiop:file-exists-p file)
      (load file))))

(defun init (args)
  (run-hooks *before-init-hook*)
  (unless (command-line-arguments-without-init-file args)
    (load-init-file))
  (run-hooks *after-init-hook*)
  (apply-args args))

(defun run-editor-thread (initialize args finalize)
  (bt2:make-thread
   (lambda ()
     (when initialize (funcall initialize))
     (unwind-protect
          (let (#+lispworks (lw:*default-character-element-type* 'character))
            (with-editor-stream ()
              (with-timer-manager (make-instance 'lem-timer-manager)
                (setf *in-the-editor* t)
                (setup)
                (let ((report (toplevel-command-loop (lambda () (init args)))))
                  (when finalize (funcall finalize report))
                  (teardown)))))
       (setf *in-the-editor* nil)))
   :name "editor"))

(defun find-editor-thread ()
  (find "editor" (bt2:all-threads)
        :test #'equal
        :key #'bt2:thread-name))

;; NOTE: so that it can be processed during compilation.
(defvar *version* (get-version-string))

(defun lem (&rest args)

  ;; for sbcl, set the default file encoding to utf-8
  ;; (on windows, the default is determined by code page (e.g. :cp932))
  #+sbcl
  (setf sb-impl::*default-external-format* :utf-8)

  (setf args (parse-args args))

  (when (command-line-arguments-help args)
    (uiop:println *help*)
    (return-from lem))

  (when (command-line-arguments-version args)
    (uiop:println *version*)
    (return-from lem))

  (cond
    ((command-line-arguments-log-filename args)
     (apply #'log:config
            :sane
            :daily (command-line-arguments-log-filename args)
            (list
             (if (command-line-arguments-debug args)
                 :debug
                 :info))))
    (t (log:config :off)))

  (log:info "Starting Lem")

  (cond (*in-the-editor*
         (apply-args args))
        (t
         (let ((implementation
                (get-default-implementation
                 :errorp nil
                 :implementation
                 (or (command-line-arguments-interface args)
                     (if (interactive-stream-p *standard-input*)
                         :ncurses
                         :sdl2)))))
           (unless implementation
             (maybe-load-systems :lem-ncurses)
             (setf implementation (get-default-implementation)))
           (invoke-frontend
            (lambda (&optional initialize finalize)
              (run-editor-thread initialize args finalize))
            :implementation implementation)))))

(defun main (&optional (args (uiop:command-line-arguments)))
  (apply #'lem args))

#+sbcl
(push #'(lambda (x)
          (if x
              (lem x)
              (lem))
          t)
      sb-ext:*ed-functions*)


(add-hook *after-init-hook*
          (lambda ()
            (when *editor-warnings*
              (let ((buffer (make-buffer "*EDITOR WARNINGS*")))
                (dolist (warning *editor-warnings*)
                  (insert-string (buffer-point buffer) warning)
                  (insert-character (buffer-point buffer) #\newline))
                (pop-to-buffer buffer)))))
