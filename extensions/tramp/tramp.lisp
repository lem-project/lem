(defpackage :lem-tramp
  (:use :cl :lem)
  (:export :tramp-mode))
(in-package :lem-tramp)

(setf (documentation *package* t)
      "TRAMP-like remote file editing for Lem.
Supports /ssh:user@host:/path and /sudo::/path syntax for transparent
remote file access via SSH and sudo.

C-x C-f /ssh:host:/etc/hostname
C-x C-f /sudo::/etc/hostname")

;;; ------------------------------------------------------------------
;;; Path Parsing
;;; ------------------------------------------------------------------

(defun tramp-path-p (filename)
  "Return T if FILENAME is a TRAMP-style path (/method:user@host:/path)."
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (and (stringp filename)
       (> (length filename) 1)
       (char= (char filename 0) #\/)
       (ppcre:scan "^\\w+:" (subseq filename 1))
       t))

(defun parse-tramp-path (filename)
  "Parse FILENAME like /ssh:user@host:/remote/path or /sudo::/path.
Returns (values method user host remote-path)."
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (ppcre:register-groups-bind (method user-host remote-path)
      ("^/(\\w+):([^:]*):(.+)" filename)
    (unless method
      (editor-error "Invalid TRAMP path: ~A" filename))
    (let ((user nil)
          (host nil))
      (when (and user-host (> (length user-host) 0))
        (if (find #\@ user-host)
            (ppcre:register-groups-bind (u h)
                ("^([^@]*)@(.*)" user-host)
              (setf user (unless (string= u "") u)
                    host (unless (string= h "") h)))
            (setf host user-host)))
      (when (null host)
        (setf host "localhost"))
      (values (intern (string-upcase method) :keyword)
              user
              host
              remote-path))))

;;; ------------------------------------------------------------------
;;; Password Management
;;; ------------------------------------------------------------------

(defvar *tramp-passwords* (make-hash-table :test 'equal)
  "Cache of passwords keyed by connection key (e.g. \"sudo:root@localhost\").")

(defun tramp-connection-key (method user host)
  "Make a cache key for a TRAMP connection."
  (format nil "~A:~A@~A" method (or user "") host))

(defun tramp-get-password (method user host)
  "Get the cached password for a connection, or nil."
  (values (gethash (tramp-connection-key method user host) *tramp-passwords*)))

(defun tramp-clear-password (method user host)
  "Clear the cached password for a connection (on auth failure)."
  (remhash (tramp-connection-key method user host) *tramp-passwords*))

(defun tramp-prompt-password (method user host)
  "Prompt the user for a password and cache it.
Returns the password string, or nil if cancelled/empty."
  (let ((prompt (format nil "TRAMP password for /~A:~@[~A@~]~A: "
                        (string-downcase method) user host)))
    (let ((password (prompt-for-string prompt)))
      (if (and password (plusp (length password)))
          (progn
            (setf (gethash (tramp-connection-key method user host) *tramp-passwords*)
                  password)
            password)
          (progn
            (tramp-clear-password method user host)
            nil)))))

(defvar *ssh-key-auth-cache* (make-hash-table :test 'equal)
  "Cache of SSH key auth results. Maps connection key to T (key works) or NIL (needs password).")

(defun sshpass-available-p ()
  "Check if the sshpass utility is available on the system."
  (or (eql 0 (nth-value 2
                        (uiop:run-program '("which" "sshpass")
                                          :output nil
                                          :error-output nil
                                          :ignore-error-status t)))
      ;; Fallback: check if sshpass exists at common locations
      (probe-file "/usr/bin/sshpass")
      (probe-file "/usr/local/bin/sshpass")))

(defun ssh-check-key-auth (user host)
  "Check if key-based SSH authentication works for USER@HOST.
Returns T if key auth works, NIL otherwise. Caches the result."
  (let* ((conn-key (format nil "~A@~A" (or user "") host))
         (cached (gethash conn-key *ssh-key-auth-cache* :not-found)))
    (when (eq cached :not-found)
      (let ((target (if user (format nil "~A@~A" user host) host)))
        (setf cached
              (eql 0 (handler-case
                         (nth-value 2
                                    (uiop:run-program
                                     `("ssh" "-T"
                                       "-o" "BatchMode=yes"
                                       "-o" "ConnectTimeout=3"
                                       "-o" "StrictHostKeyChecking=accept-new"
                                       ,@(ssh-control-options)
                                       ,target "true")
                                     :output nil
                                     :error-output nil
                                     :ignore-error-status t))
                       (error () 1))))
        (setf (gethash conn-key *ssh-key-auth-cache*) cached)))
    cached))

(defun tramp-ensure-password (method user host)
  "Get cached password or prompt the user. Returns nil if no password needed/available."
  (or (tramp-get-password method user host)
      (ecase method
        (:sudo
         (or (tramp-prompt-password method user host)
             (error 'editor-abort)))
        (:ssh
         (unless (ssh-check-key-auth user host)
           (if (sshpass-available-p)
               (tramp-prompt-password method user host)
               (editor-error
                "SSH key authentication failed for ~A@~A.~%~
                  Install sshpass for password authentication, or set up SSH keys."
                (or user "") host)))))))

;;; ------------------------------------------------------------------
;;; Remote Command Execution
;;; ------------------------------------------------------------------

(defun ssh-control-options ()
  "Return SSH ControlMaster options for connection multiplexing.
All SSH connections to the same host reuse a single TCP connection,
avoiding repeated authentication delays."
  (list "-o" "ControlMaster=auto"
        "-o" "ControlPath=/tmp/lem-ssh-%C"
        "-o" "ControlPersist=60"))

(defun build-ssh-args (method user host command &key (use-sudo-s nil) password)
  "Build the argument list for running a command via SSH or sudo.
When USE-SUDO-S is T, sudo reads the password from stdin (-S flag).
When PASSWORD is provided for :ssh, uses sshpass to deliver the password."
  (ecase method
    (:ssh
     (let ((target (if user (format nil "~A@~A" user host) host))
           (cmd-args (if (listp command) command (list command)))
           (control-opts (ssh-control-options)))
       (if password
           `("sshpass" "-p" ,password
             "ssh" "-T"
             "-o" "StrictHostKeyChecking=accept-new"
             "-o" "ConnectTimeout=3"
             ,@control-opts
             ,target ,@cmd-args)
           `("ssh" "-T"
             "-o" "BatchMode=yes"
             "-o" "StrictHostKeyChecking=accept-new"
             "-o" "ConnectTimeout=3"
             ,@control-opts
             ,target ,@cmd-args))))
    (:sudo
     (let ((args (list "sudo")))
       (when use-sudo-s
         (push "-S" (cdr args)))          ; -S: read password from stdin
       (when user
         (setf args (append args (list "-u" user))))
       (append args (if (listp command) command (list command)))))))

(defun %run-remote-with-password (method user host args
                                  &key (output :string) input (error-output :string))
  "Run a command with optional password authentication.
For :sudo, sends password via stdin with sudo -S.
For :ssh, password is passed via sshpass in the args list.
Returns (values process-info password-input-stream-or-nil)."
  (let ((password (tramp-ensure-password method user host)))
    (if password
        (ecase method
          (:sudo
           ;; sudo: password via stdin with -S
           (handler-case
               (let ((process
                       (uiop:launch-program args
                                            :output output
                                            :input :stream
                                            :error-output error-output
                                            :ignore-error-status t)))
                 (let ((pwd-stream (uiop:process-info-input process)))
                   (write-line password pwd-stream)
                   (finish-output pwd-stream)
                   (values process pwd-stream)))
             (editor-error (e)
               (error e))
             (error (e)
               (tramp-clear-password method user host)
               (editor-error "Failed to run remote command ~A: ~A" args e))))
          (:ssh
           ;; ssh: password already in args via sshpass prefix (build-ssh-args)
           (handler-case
               (values (uiop:launch-program args
                                            :output output
                                            :input input
                                            :error-output error-output
                                            :ignore-error-status t)
                       nil)
             (editor-error (e)
               (error e))
             (error (e)
               (tramp-clear-password method user host)
               (editor-error "Failed to run remote command ~A: ~A" args e)))))
        ;; No password needed
        (handler-case
            (values (uiop:launch-program args
                                         :output output
                                         :input input
                                         :error-output error-output
                                         :ignore-error-status t)
                    nil)
          (editor-error (e)
            (error e))
          (error (e)
            (editor-error "Failed to run remote command ~A: ~A" args e))))))

(defun run-remote (method user host command &key (output :string) input (error-output :string))
  "Run a command on a remote host via SSH or sudo.
Returns the process-info from uiop:launch-program.
For sudo and SSH, automatically handles password prompting if needed."
  (let* ((password (tramp-ensure-password method user host))
         (use-sudo-s (and (eq method :sudo) password))
         (args (build-ssh-args method user host command
                               :use-sudo-s use-sudo-s
                               :password (when (eq method :ssh) password))))
    (multiple-value-bind (process pwd-stream)
        (%run-remote-with-password method user host args
                                   :output output :input input :error-output error-output)
      (declare (ignore pwd-stream))
      process)))

(defun run-remote-string (method user host command)
  "Run a command on a remote host and return its stdout as a string."
  (let* ((password (tramp-ensure-password method user host))
         (use-sudo-s (and (eq method :sudo) password))
         (args (build-ssh-args method user host command
                               :use-sudo-s use-sudo-s
                               :password (when (eq method :ssh) password))))
    (handler-case
        (if (and password (eq method :sudo))
            ;; sudo with password: use launch-program to avoid temp file issues
            (let* ((process (uiop:launch-program args
                                                 :output :stream
                                                 :input :stream
                                                 :error-output :stream
                                                 :ignore-error-status t))
                   (in (uiop:process-info-input process))
                   (out (uiop:process-info-output process)))
              (write-line password in)
              (finish-output in)
              (close in)
              (let ((result
                    (with-output-to-string (s)
                      (loop for line = (read-line out nil nil)
                            while line
                            do (write-line line s)))))
              (ignore-errors (close out))
              (uiop:wait-process process)
              (string-trim '(#\newline #\return) result)))
            (string-trim
             '(#\newline #\return)
             (uiop:run-program args
                               :output :string
                               :error-output :string
                               :ignore-error-status t)))
      (editor-error (e)
        (error e))
      (error (e)
        (tramp-clear-password method user host)
        (editor-error "Failed to run remote command ~A: ~A" command e)))))

(defun run-remote-exit-code (method user host command)
  "Run a command on a remote host and return its exit code (0 = success)."
  (let* ((password (tramp-ensure-password method user host))
         (use-sudo-s (and (eq method :sudo) password))
         (args (build-ssh-args method user host command
                               :use-sudo-s use-sudo-s
                               :password (when (eq method :ssh) password))))
    (handler-case
        (if (and password (eq method :sudo))
            ;; sudo with password: use launch-program with pipe for stdin
            ;; (avoids uiop:run-program temp file issues)
            (let* ((process (uiop:launch-program args
                                                 :output nil
                                                 :input :stream
                                                 :error-output nil
                                                 :ignore-error-status t))
                   (in (uiop:process-info-input process)))
              (write-line password in)
              (finish-output in)
              (close in)
              (uiop:wait-process process))
            (nth-value 2
                       (uiop:run-program args
                                         :output nil
                                         :error-output nil
                                         :ignore-error-status t)))
      (editor-error (e)
        (error e))
      (error (c)
        (tramp-clear-password method user host)
        (message "TRAMP: sudo failed - ~A" c)
        nil))))

;;; ------------------------------------------------------------------
;;; Stream Creation
;;; ------------------------------------------------------------------

(defun %make-ssh-output-stream (method user host path)
  "Create a stream for reading a remote file via SSH/sudo.
Reads the entire file content into memory via uiop:run-program,
avoiding pipe/process lifetime issues.
Returns (values stream closer-fn)."
  (let* ((cmd (ecase method
                (:ssh `("cat" ,path))
                (:sudo `("cat" ,path))))
         (use-sudo-s (eq method :sudo))
         (password (tramp-ensure-password method user host))
         (args (build-ssh-args method user host cmd
                               :use-sudo-s use-sudo-s
                               :password (when (eq method :ssh) password))))
    (handler-case
        (let* ((output
                 (if (and password (eq method :sudo))
                     (with-input-from-string (s (format nil "~A~%" password))
                       (uiop:run-program args
                                         :output :string
                                         :input s
                                         :error-output :string
                                         :ignore-error-status t))
                     (uiop:run-program args
                                       :output :string
                                       :error-output :string
                                       :ignore-error-status t)))
               (octets (babel:string-to-octets output :encoding :utf-8)))
          (values (flexi-streams:make-in-memory-input-stream octets)
                  (lambda (s)
                    (declare (ignore s)))))
      (editor-error (e)
        (error e))
      (error (e)
        (tramp-clear-password method user host)
        (editor-error "Failed to read remote file ~A: ~A" path e)))))

(defun %make-ssh-input-stream (method user host path)
  "Create a stream for writing a remote file via SSH/sudo.
Returns (values stream closer-fn)."
  (let* ((cmd (ecase method
                (:ssh (list "/bin/sh" "-c"
                            (format nil "cat > ~A" (escape-shell-arg path))))
                (:sudo (list "/bin/sh" "-c"
                             (format nil "cat > ~A" (escape-shell-arg path))))))
         (use-sudo-s (eq method :sudo))
         (password (tramp-ensure-password method user host))
         (args (build-ssh-args method user host cmd
                               :use-sudo-s use-sudo-s
                               :password (when (eq method :ssh) password))))
    (handler-case
        (let* ((process (uiop:launch-program args
                                             :output nil
                                             :input :stream
                                             :error-output :stream
                                             :ignore-error-status t))
               (stream (uiop:process-info-input process)))
          ;; For sudo: send password via stdin first
          (when (and password (eq method :sudo))
            (write-line password stream)
            (finish-output stream))
          ;; For ssh: password already in args via sshpass, nothing extra needed
          (values stream
                  (lambda (s)
                    ;; s is the flexi-stream (or raw stream for binary);
                    ;; flush it so all buffered data reaches the pipe
                    (finish-output s)
                    (ignore-errors (close s))
                    ;; wait for ssh to finish writing to the remote file
                    (ignore-errors (uiop:wait-process process)))))
      (editor-error (e)
        (error e))
      (error (e)
        (tramp-clear-password method user host)
        (editor-error "Failed to write remote file ~A: ~A" path e)))))

(defun escape-shell-arg (arg)
  "Escape ARG for safe use in a shell command (single-quote escaping)."
  (let ((escaped (ppcre:regex-replace-all "'" arg "'\\''")))
    (concatenate 'string "'" escaped "'")))

;;; ------------------------------------------------------------------
;;; Virtual File Open Handler
;;; ------------------------------------------------------------------

(defun tramp-file-open-handler (filename &key direction element-type external-format)
  "Handler for *virtual-file-open* that intercepts TRAMP paths."
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (when (tramp-path-p filename)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path filename)
      (ecase direction
        (:input
         (multiple-value-bind (raw-stream closer)
             (%make-ssh-output-stream method user host remote-path)
           (if (equal element-type '(unsigned-byte 8))
               ;; Binary stream — return as-is
               (list raw-stream closer)
               ;; Character stream — need to wrap
               (list (flexi-streams:make-flexi-stream raw-stream
                                                      :external-format (or external-format :utf-8))
                     closer))))
        (:output
         (multiple-value-bind (raw-stream closer)
             (%make-ssh-input-stream method user host remote-path)
           (if (equal element-type '(unsigned-byte 8))
               (list raw-stream closer)
               (list (flexi-streams:make-flexi-stream raw-stream
                                                      :external-format (or external-format :utf-8))
                     closer))))))))

;;; ------------------------------------------------------------------
;;; Filesystem Hooks
;;; ------------------------------------------------------------------

(defun tramp-probe-file-handler (pathspec &optional base-dir)
  "Handler for *virtual-probe-file-functions*."
  (declare (ignore base-dir))
  (when (tramp-path-p pathspec)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path pathspec)
      ;; Use exit code instead of shell && to avoid needing shell
      (let ((code (run-remote-exit-code method user host
                                        (list "test" "-f" remote-path))))
        (when (eql 0 code)
          ;; Return as string — namestring on a string is identity
          (namestring pathspec))))))

(defun tramp-directory-exists-handler (directory)
  "Handler for *virtual-directory-exists-p-functions*."
  (when (tramp-path-p directory)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path directory)
      (let ((code (run-remote-exit-code method user host
                                        (list "test" "-d" remote-path))))
        (when (eql 0 code)
          directory)))))

(defun tramp-directory-files-handler (pathspec)
  "Handler for *virtual-directory-files-functions*.
If PATHSPEC is a file (not a directory), return a list of just that file.
If it's a directory, list its contents via ls."
  (when (tramp-path-p pathspec)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path pathspec)
      ;; First check if this is a directory
      (if (eql 0 (run-remote-exit-code method user host
                                       (list "test" "-d" remote-path)))
          ;; It's a directory — list its contents
          (let ((output (run-remote-string method user host
                                           (list "ls" "-1a" remote-path))))
            (when output
              (let ((prefix (if (char= (char pathspec (1- (length pathspec))) #\/)
                                pathspec
                                (concatenate 'string pathspec "/"))))
                (loop :for line :in (str:lines output)
                      :for name := (string-trim '(#\space #\tab) line)
                      :unless (or (string= name "") (string= name ".") (string= name ".."))
                      :collect (concatenate 'string prefix name)))))
          ;; It's a file (or doesn't exist) — return as-is like local behavior
          (list pathspec)))))

(defun tramp-file-metadata-handler (pathname op)
  "Handler for *virtual-file-metadata-functions*."
  (when (tramp-path-p pathname)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path pathname)
      (ecase op
        (:size
         (let ((result (run-remote-string method user host
                                          (list "stat" "-c" "%s" remote-path))))
           (when result
             (ignore-errors (parse-integer result)))))
        (:mtime
         (let ((result (run-remote-string method user host
                                          (list "stat" "-c" "%Y" remote-path))))
           (when result
             (ignore-errors (parse-integer result)))))
        (:write-date
         (let ((result (run-remote-string method user host
                                          (list "stat" "-c" "%Y" remote-path))))
           (when result
             (ignore-errors (parse-integer result)))))))))

(defun tramp-expand-file-name-handler (filename directory)
  "Handler for *virtual-expand-file-name-functions*.
For TRAMP paths, skip local path merging and return the path as-is."
  (declare (ignore directory))
  (when (tramp-path-p filename)
    filename))

;;; ------------------------------------------------------------------
;;; External Format Detection Override
;;; ------------------------------------------------------------------

(defvar *tramp-original-external-format-function* nil
  "Saved original value of *external-format-function* before TRAMP overrides it.")

(defun tramp-external-format-function-wrapper (filename)
  "Wrapper for *external-format-function* that handles TRAMP paths.
TRAMP files cannot be opened with CL's OPEN for encoding detection,
so we return a safe default (:utf-8 :lf) for remote files."
  (if (tramp-path-p filename)
      (values :utf-8 :lf)
      (if *tramp-original-external-format-function*
          (funcall *tramp-original-external-format-function* filename)
          (values :utf-8 :lf))))

;;; ------------------------------------------------------------------
;;; Registration
;;; ------------------------------------------------------------------

(defun tramp-enable ()
  "Enable TRAMP remote file support."
  (pushnew 'tramp-file-open-handler *virtual-file-open*)
  (pushnew 'tramp-probe-file-handler
           lem/buffer/file-utils:*virtual-probe-file-functions*)
  (pushnew 'tramp-directory-exists-handler
           lem/buffer/file-utils:*virtual-directory-exists-p-functions*)
  (pushnew 'tramp-directory-files-handler
           lem/buffer/file-utils:*virtual-directory-files-functions*)
  (pushnew 'tramp-file-metadata-handler
           lem/buffer/file-utils:*virtual-file-metadata-functions*)
  (pushnew 'tramp-expand-file-name-handler
           lem/buffer/file-utils:*virtual-expand-file-name-functions*)
  ;; Override encoding detection for TRAMP paths: inq:detect-encoding
  ;; calls CL:OPEN directly (bypassing the virtual filesystem), so we
  ;; must intercept *external-format-function* to return :utf-8 for
  ;; remote files instead of trying to open them locally.
  (unless *tramp-original-external-format-function*
    (setf *tramp-original-external-format-function*
          lem/buffer/file:*external-format-function*)
    (setf lem/buffer/file:*external-format-function*
          'tramp-external-format-function-wrapper)))

(defun tramp-disable ()
  "Disable TRAMP remote file support."
  (setf *virtual-file-open*
        (remove 'tramp-file-open-handler *virtual-file-open*))
  (setf lem/buffer/file-utils:*virtual-probe-file-functions*
        (remove 'tramp-probe-file-handler lem/buffer/file-utils:*virtual-probe-file-functions*))
  (setf lem/buffer/file-utils:*virtual-directory-exists-p-functions*
        (remove 'tramp-directory-exists-handler lem/buffer/file-utils:*virtual-directory-exists-p-functions*))
  (setf lem/buffer/file-utils:*virtual-directory-files-functions*
        (remove 'tramp-directory-files-handler lem/buffer/file-utils:*virtual-directory-files-functions*))
  (setf lem/buffer/file-utils:*virtual-file-metadata-functions*
        (remove 'tramp-file-metadata-handler lem/buffer/file-utils:*virtual-file-metadata-functions*))
  (setf lem/buffer/file-utils:*virtual-expand-file-name-functions*
        (remove 'tramp-expand-file-name-handler lem/buffer/file-utils:*virtual-expand-file-name-functions*))
  ;; Restore original encoding detection function
  (when *tramp-original-external-format-function*
    (setf lem/buffer/file:*external-format-function*
          *tramp-original-external-format-function*)
    (setf *tramp-original-external-format-function* nil)))

;; Auto-enable at load time
(tramp-enable)
