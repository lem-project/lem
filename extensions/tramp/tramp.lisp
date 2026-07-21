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
      ("^/(\\w+):([^:]*):(.*)" filename)
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
      (when (or (null remote-path) (string= remote-path ""))
        (setf remote-path "/"))
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

;;; ------------------------------------------------------------------
;;; FS Cache (5-second TTL — eliminates duplicate SSH calls)
;;; ------------------------------------------------------------------

(defvar *tramp-fs-cache* (make-hash-table :test 'equal)
  "Cache for filesystem operations. Keys are (method user host path op),
values are cons of (timestamp . result).")

(defvar *tramp-fs-cache-ttl* 5
  "Time-to-live in seconds for filesystem cache entries.")

(defun tramp-fs-cache-key (method user host path op)
  "Make a cache key for a filesystem operation."
  (format nil "~A:~A@~A:~A:~A" method (or user "") host path op))

(defun tramp-fs-cache-get (method user host path op)
  "Get a cached value, or :not-found."
  (let* ((key (tramp-fs-cache-key method user host path op))
         (entry (gethash key *tramp-fs-cache*)))
    (if (and entry (< (- (get-universal-time) (car entry)) *tramp-fs-cache-ttl*))
        (cdr entry)
        (progn (remhash key *tramp-fs-cache*) :not-found))))

(defun tramp-fs-cache-set (method user host path op value)
  "Cache a value with current timestamp."
  (setf (gethash (tramp-fs-cache-key method user host path op) *tramp-fs-cache*)
        (cons (get-universal-time) value)))

;;; ------------------------------------------------------------------
;;; SSH Auth (lazy — no separate pre-check call)
;;; ------------------------------------------------------------------

(defvar *ssh-auth-method-cache* (make-hash-table :test 'equal)
  "Cache of SSH auth methods. Values: :key (key auth works),
:password (needs password), or nil (unknown).")

(defun sshpass-available-p ()
  "Check if the sshpass utility is available on the system."
  (or (eql 0 (nth-value 2
                        (uiop:run-program '("which" "sshpass")
                                          :output nil
                                          :error-output nil
                                          :ignore-error-status t)))
      (probe-file "/usr/bin/sshpass")
      (probe-file "/usr/local/bin/sshpass")))

(defun ssh-ensure-auth (method user host)
  "Get cached auth state for SSH connection.
Returns (values password auth-tried-p):
  - cached password → (values password t)
  - key auth known to work → (values nil t)
  - unknown → (values nil nil) — caller should try BatchMode first"
  (declare (ignore method))
  (let ((conn-key (format nil "~A@~A" (or user "") host)))
    (or (let ((pwd (tramp-get-password :ssh user host)))
          (when pwd (return-from ssh-ensure-auth (values pwd t))))
        (let ((auth-method (gethash conn-key *ssh-auth-method-cache*)))
          (ecase auth-method
            ((nil) (values nil nil))         ;; unknown — try key first
            (:key (values nil t))             ;; key works, no password needed
            (:password                        ;; need password
             (if (sshpass-available-p)
                 (let ((pwd (tramp-prompt-password :ssh user host)))
                   (values pwd t))
                 (editor-error
                  "SSH key auth failed for ~A@~A. Install sshpass for password auth."
                  (or user "") host))))))))

(defun ssh-remember-auth-failure (user host)
  "Called when a BatchMode SSH command fails (exit 255).
Marks connection as needing password and prompts."
  (let ((conn-key (format nil "~A@~A" (or user "") host)))
    (setf (gethash conn-key *ssh-auth-method-cache*) :password)
    (tramp-clear-password :ssh user host)
    (if (sshpass-available-p)
        (tramp-prompt-password :ssh user host)
        (editor-error
         "SSH key auth failed for ~A@~A. Install sshpass for password auth."
         (or user "") host))))

(defun ssh-remember-auth-success (user host)
  "Called when a BatchMode SSH command succeeds. Marks key auth as working."
  (let ((conn-key (format nil "~A@~A" (or user "") host)))
    (setf (gethash conn-key *ssh-auth-method-cache*) :key)))

(defun tramp-ensure-password (method user host)
  "Get cached password or prompt the user. For :ssh returns nil
(lazy auth — the actual command will trigger auth handling)."
  (or (tramp-get-password method user host)
      (ecase method
        (:sudo
         (if (eql 0 (nth-value 2
                               (uiop:run-program '("sudo" "-n" "true")
                                                 :output nil
                                                 :error-output nil
                                                 :ignore-error-status t)))
             nil  ;; passwordless sudo
             (or (tramp-prompt-password method user host)
                 (error 'editor-abort))))
        (:ssh
         ;; Lazy auth: authenticated on first actual command, not here
         nil))))

;;; ------------------------------------------------------------------
;;; Remote Command Execution
;;; ------------------------------------------------------------------

(defun ssh-control-options ()
  "Return SSH ControlMaster options for connection multiplexing."
  (list "-o" "ControlMaster=auto"
        "-o" "ControlPath=/tmp/lem-ssh-%C"
        "-o" "ControlPersist=60"))

(defun build-ssh-args (method user host command &key (use-sudo-s nil) password)
  "Build the argument list for running a command via SSH or sudo."
  (ecase method
    (:ssh
     (let ((target (if user (format nil "~A@~A" user host) host))
           (cmd-args (if (listp command) command (list command)))
           (control-opts (ssh-control-options)))
       (if password
           (append (list "sshpass" "-p" password
                         "ssh" "-T"
                         "-o" "StrictHostKeyChecking=accept-new"
                         "-o" "ConnectTimeout=3")
                   control-opts
                   (list target)
                   cmd-args)
           (append (list "ssh" "-T"
                         "-o" "BatchMode=yes"
                         "-o" "StrictHostKeyChecking=accept-new"
                         "-o" "ConnectTimeout=3")
                   control-opts
                   (list target)
                   cmd-args))))
    (:sudo
     (let ((args (list "sudo")))
       (when use-sudo-s
         (push "-S" (cdr args)))
       (when user
         (setf args (append args (list "-u" user))))
       (append args (if (listp command) command (list command)))))))

;;; Core SSH execution with lazy auth

(defun %ssh-run (user host args)
  "Run an SSH command, returning (values exit-code stdout-string)."
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program args
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore stderr))
        (values exit-code stdout))
    (error (c)
      (values 255 (princ-to-string c)))))

(defun %ssh-run-with-auth-retry (user host command)
  "Run an SSH command with automatic auth handling.
Tries key auth first; on exit-255 failure, prompts for password and retries.
Returns (values exit-code stdout-string)."
  (multiple-value-bind (password auth-tried) (ssh-ensure-auth :ssh user host)
    (if auth-tried
        ;; Auth method known — run directly
        (let ((args (build-ssh-args :ssh user host command :password password)))
          (%ssh-run user host args))
        ;; Auth method unknown — try key auth first
        (let ((args (build-ssh-args :ssh user host command :password nil)))
          (multiple-value-bind (exit-code stdout) (%ssh-run user host args)
            (if (= exit-code 255)
                ;; Auth failure → prompt password and retry
                (let ((pwd (ssh-remember-auth-failure user host)))
                  (if pwd
                      (let ((args2 (build-ssh-args :ssh user host command :password pwd)))
                        (%ssh-run user host args2))
                      (values exit-code stdout)))
                ;; Key auth succeeded or command failed for other reasons
                (progn
                  (ssh-remember-auth-success user host)
                  (values exit-code stdout))))))))

;;; Sudo command execution (pipe-based, no temp files)

(defun %sudo-run (user host args password)
  "Run a sudo command via pipe. Returns (values exit-code stdout-string)."
  (handler-case
      (let* ((process (uiop:launch-program args
                                           :output :stream
                                           :input :stream
                                           :error-output :stream
                                           :ignore-error-status t))
             (in (uiop:process-info-input process))
             (out (uiop:process-info-output process)))
        (when password
          (write-line password in)
          (finish-output in))
        (close in)
        (let ((stdout
                (with-output-to-string (s)
                  (loop :for line := (read-line out nil nil)
                        :while line
                        :do (write-line line s)))))
          (ignore-errors (close out))
          (let ((exit-code (uiop:wait-process process)))
            (values exit-code stdout))))
    (error (c)
      (values 1 (princ-to-string c)))))

(defun %sudo-run-exit-code (user host args password)
  "Run a sudo command, returning just the exit code."
  (handler-case
      (let* ((process (uiop:launch-program args
                                           :output nil
                                           :input :stream
                                           :error-output nil
                                           :ignore-error-status t))
             (in (uiop:process-info-input process)))
        (when password
          (write-line password in)
          (finish-output in))
        (close in)
        (uiop:wait-process process))
    (error () 1)))

;;; Public API

(defun run-remote-exit-code (method user host command)
  "Run a command on a remote host. Returns its exit code (0 = success)."
  (if (eq method :ssh)
      (%ssh-run-with-auth-retry user host command)
      (let* ((password (tramp-ensure-password method user host))
             (use-sudo-s (and (eq method :sudo) password))
             (args (build-ssh-args method user host command :use-sudo-s use-sudo-s)))
        (%sudo-run-exit-code user host args password))))

(defun run-remote-string (method user host command)
  "Run a command on a remote host. Returns its stdout as a trimmed string."
  (if (eq method :ssh)
      (multiple-value-bind (exit-code stdout)
          (%ssh-run-with-auth-retry user host command)
        (declare (ignore exit-code))
        (string-trim '(#\newline #\return) stdout))
      (let* ((password (tramp-ensure-password method user host))
             (use-sudo-s (and (eq method :sudo) password))
             (args (build-ssh-args method user host command :use-sudo-s use-sudo-s)))
        (multiple-value-bind (exit-code stdout)
            (%sudo-run user host args password)
          (declare (ignore exit-code))
          (string-trim '(#\newline #\return) stdout)))))

;;; ------------------------------------------------------------------
;;; Stream Creation
;;; ------------------------------------------------------------------

(defun %read-remote-file (method user host path)
  "Read a remote file via cat. Returns the content as a string."
  (handler-case
      (let ((cmd (list "cat" path)))
        (if (eq method :ssh)
            (multiple-value-bind (exit-code stdout)
                (%ssh-run-with-auth-retry user host cmd)
              (unless (eql 0 exit-code)
                (editor-error "Failed to read remote file ~A (exit ~D)" path exit-code))
              stdout)
            (let* ((password (tramp-ensure-password method user host))
                   (use-sudo-s (and (eq method :sudo) password))
                   (args (build-ssh-args method user host cmd :use-sudo-s use-sudo-s)))
              (multiple-value-bind (exit-code stdout)
                  (%sudo-run user host args password)
                (unless (eql 0 exit-code)
                  (editor-error "Failed to read remote file ~A (exit ~D)" path exit-code))
                ;; Guard: strip password if it leaked into stdout due to
                ;; a lem-webview prompt overlay cleanup race.
                (when (and password (plusp (length password)))
                  (when (str:starts-with-p password stdout)
                    (setf stdout (subseq stdout (length password)))
                    (setf stdout (string-left-trim '(#\newline #\return) stdout))))
                stdout))))
    (editor-error (e)
      (error e))
    (error (e)
      (tramp-clear-password method user host)
      (editor-error "Failed to read remote file ~A: ~A" path e))))

(defun %make-ssh-output-stream (method user host path)
  "Create an in-memory stream for reading a remote file."
  (let ((output (%read-remote-file method user host path)))
    (let ((octets (babel:string-to-octets output :encoding :utf-8)))
      (values (flexi-streams:make-in-memory-input-stream octets)
              (lambda (s)
                (declare (ignore s)))))))

(defun %make-ssh-input-stream (method user host path)
  "Create a stream for writing a remote file via SSH/sudo."
  (let* ((cmd (ecase method
                (:ssh (list "/bin/sh" "-c"
                            (format nil "cat > ~A" (escape-shell-arg path))))
                (:sudo (list "/bin/sh" "-c"
                             (format nil "cat > ~A" (escape-shell-arg path))))))
         (use-sudo-s (eq method :sudo))
         (password (or (tramp-get-password method user host)
                       (tramp-ensure-password method user host)))
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
          (when (and password (eq method :sudo))
            (write-line password stream)
            (finish-output stream))
          (values stream
                  (lambda (s)
                    (finish-output s)
                    (ignore-errors (close s))
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
               (list raw-stream closer)
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
;;; Filesystem Hooks (with 5-second FS cache)
;;; ------------------------------------------------------------------

(defun tramp-probe-file-handler (pathspec &optional base-dir)
  "Handler for *virtual-probe-file-functions*. Uses cache to avoid duplicate SSH calls."
  (declare (ignore base-dir))
  (when (tramp-path-p pathspec)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path pathspec)
      (let ((cached (tramp-fs-cache-get method user host remote-path :probe-file)))
        (unless (eq cached :not-found)
          (return-from tramp-probe-file-handler cached)))
      (let ((code (run-remote-exit-code method user host
                                        (list "test" "-f" remote-path))))
        (let ((result (when (eql 0 code) (namestring pathspec))))
          (tramp-fs-cache-set method user host remote-path :probe-file result)
          result)))))

(defun tramp-directory-exists-handler (directory)
  "Handler for *virtual-directory-exists-p-functions*. Uses cache."
  (when (tramp-path-p directory)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path directory)
      (let ((cached (tramp-fs-cache-get method user host remote-path :dir-exists)))
        (unless (eq cached :not-found)
          (return-from tramp-directory-exists-handler
            (when cached directory))))
      (let ((code (run-remote-exit-code method user host
                                        (list "test" "-d" remote-path))))
        (tramp-fs-cache-set method user host remote-path :dir-exists (eql 0 code))
        (when (eql 0 code)
          directory)))))

(defun tramp-directory-files-handler (pathspec)
  "Handler for *virtual-directory-files-functions*.
Caches directory check and listings for 5 seconds.
For :sudo, delegates to local filesystem (no remote calls) so
completion works without triggering a password prompt."
  (when (tramp-path-p pathspec)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path pathspec)
      (when (eq method :sudo)
        (return-from tramp-directory-files-handler
          (tramp-sudo-directory-files pathspec remote-path)))
      ;; Use cached directory check
      (let ((cached (tramp-fs-cache-get method user host remote-path :dir-exists)))
        (if (eq cached :not-found)
            ;; Check and cache
            (let ((is-dir (eql 0 (run-remote-exit-code method user host
                                                       (list "test" "-d" remote-path)))))
              (tramp-fs-cache-set method user host remote-path :dir-exists is-dir)
              (if is-dir
                  (tramp-list-directory-1 method user host pathspec remote-path)
                  (list pathspec)))
            (if cached
                (tramp-list-directory-1 method user host pathspec remote-path)
                (list pathspec)))))))

(defun tramp-sudo-directory-files (pathspec remote-path)
  "List local directory contents for a :sudo path.
Uses local filesystem, not sudo — this is for completion only;
file open still goes through sudo for access."
  (when (pathnamep pathspec)
    (setf pathspec (namestring pathspec)))
  (let* ((local-dir (uiop:ensure-directory-pathname remote-path))
         (files (ignore-errors
                  (or (append (uiop:subdirectories local-dir)
                              (uiop:directory-files local-dir))
                      (directory (make-pathname :defaults local-dir
                                                :name :wild :type :wild))))))
    (when files
      (let ((prefix (if (char= (char pathspec (1- (length pathspec))) #\/)
                        pathspec
                        (concatenate 'string pathspec "/"))))
        (mapcar (lambda (f) (concatenate 'string prefix
                                         (namestring (enough-namestring f local-dir))))
                files)))))

(defun tramp-list-directory-1 (method user host pathspec remote-path)
  "List contents of a remote directory. Uses cache."
  (when (pathnamep pathspec)
    (setf pathspec (namestring pathspec)))
  (let ((cached (tramp-fs-cache-get method user host remote-path :dir-files)))
    (unless (eq cached :not-found)
      (return-from tramp-list-directory-1 cached)))
  (let ((output (run-remote-string method user host
                                   (list "ls" "-1a" remote-path))))
    (when output
      (let ((prefix (if (char= (char pathspec (1- (length pathspec))) #\/)
                        pathspec
                        (concatenate 'string pathspec "/")))
            (result '()))
        (dolist (line (str:lines output))
          (let ((name (string-trim '(#\space #\tab) line)))
            (unless (or (string= name "") (string= name ".") (string= name ".."))
              (push (concatenate 'string prefix name) result))))
        (setf result (nreverse result))
        (tramp-fs-cache-set method user host remote-path :dir-files result)
        result))))

(defun tramp-file-metadata-handler (pathname op)
  "Handler for *virtual-file-metadata-functions*.
Uses cache; fetches all metadata in a single stat call."
  (when (tramp-path-p pathname)
    (multiple-value-bind (method user host remote-path) (parse-tramp-path pathname)
      ;; Check cache for any metadata op
      (let ((cached (tramp-fs-cache-get method user host remote-path :metadata)))
        (when (eq cached :not-found)
          ;; Fetch all metadata in one call: "stat -c '%s %Y'"
          (let ((output (run-remote-string method user host
                                           (list "stat" "-c" "%s %Y" remote-path))))
            (setf cached
                  (when output
                    (let ((parts (str:split " " output :limit 2)))
                      (when (= 2 (length parts))
                        (cons (ignore-errors (parse-integer (first parts)))
                              (ignore-errors (parse-integer (second parts))))))))
            (tramp-fs-cache-set method user host remote-path :metadata cached)))
        (ecase op
          (:size (or (car cached) 0))
          (:mtime (or (cdr cached) 0))
          (:write-date (or (cdr cached) 0)))))))

(defun tramp-expand-file-name-handler (filename directory)
  "Handler for *virtual-expand-file-name-functions*.
For TRAMP paths, skip local path merging and return the path as-is."
  (declare (ignore directory))
  (when (tramp-path-p filename)
    filename))

;;; ------------------------------------------------------------------
;;; File Completion (bypasses list-directory which lacks virtual hooks)
;;; ------------------------------------------------------------------

(defvar *tramp-original-completion-function* nil
  "Saved original value of *prompt-file-completion-function*.")

(defun tramp-file-completion (string directory &key directory-only)
  "Completion function for TRAMP paths.
Bypasses list-directory (no virtual hooks) by calling
directory-files directly for the TRAMP directory listing."
  (declare (ignore directory-only))
  (let* ((expanded (expand-file-name string directory))
         (input-dir (directory-namestring expanded)))
    (if (tramp-path-p input-dir)
        (let* ((files (directory-files expanded))
               (prefix-len (length input-dir)))
          (when files
            (mapcar (lambda (f)
                      (let* ((full (namestring f))
                             (label (if (> (length full) prefix-len)
                                        (subseq full prefix-len)
                                        full)))
                        (lem/completion-mode:make-completion-item
                         :label label)))
                    files)))
        (funcall *tramp-original-completion-function*
                 string directory :directory-only directory-only))))

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
  (unless *tramp-original-external-format-function*
    (setf *tramp-original-external-format-function*
          lem/buffer/file:*external-format-function*)
    (setf lem/buffer/file:*external-format-function*
          'tramp-external-format-function-wrapper))
  ;; Override completion to handle TRAMP paths
  (unless *tramp-original-completion-function*
    (setf *tramp-original-completion-function*
          lem-core::*prompt-file-completion-function*)
    (setf lem-core::*prompt-file-completion-function*
          'tramp-file-completion)))

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
  (when *tramp-original-external-format-function*
    (setf lem/buffer/file:*external-format-function*
          *tramp-original-external-format-function*)
    (setf *tramp-original-external-format-function* nil))
  (when *tramp-original-completion-function*
    (setf lem-core::*prompt-file-completion-function*
          *tramp-original-completion-function*)
    (setf *tramp-original-completion-function* nil)))

;; Auto-enable at load time
(tramp-enable)
