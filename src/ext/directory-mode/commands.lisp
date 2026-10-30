(uiop:define-package :lem/directory-mode/commands
  (:use :cl
        :lem
        :lem/directory-mode/internal
        :lem/directory-mode/attributes)
  (:import-from :lem/directory-mode/mode
                :directory-mode)
  (:import-from :lem/directory-mode/file
                :delete-file*
                :rename-file*
                :copy-files
                :rename-files)
  (:export :directory-mode-update-buffer
           :directory-mode-up-directory
           :directory-mode-find-file
           :directory-mode-read-file
           :directory-mode-find-file-next-window
           :directory-mode-next-line
           :directory-mode-previous-line
           :directory-mode-mark-and-next-line
           :directory-mode-unmark-and-next-line
           :directory-mode-unmark-and-previous-line
           :directory-mode-toggle-marks
           :directory-mode-unmark-all
           :directory-mode-mark-regexp
           :directory-mode-mark-directories
           :directory-mode-mark-links
           :directory-mode-mark-suffix
           :directory-mode-mark-extension
           :directory-mode-next-mark
           :directory-mode-previous-mark
           :directory-mode-query-replace
           :directory-mode-query-replace-regexp
           :directory-mode-query-replace-symbol
           :directory-mode-delete-files
           :directory-mode-copy-files
           :directory-mode-rename-files
           :directory-mode-do-async-shell-command
           :directory-mode-rename-file
           :directory-mode-sort-files
           :directory-mode-sort-by-name
           :directory-mode-sort-by-time
           :directory-mode-sort-by-size
           :directory-mode-sort-by-extension
           :directory-mode-sort-reverse
           :make-directory
           :find-file-directory
           :directory-mode-kill-lines))
(in-package :lem/directory-mode/commands)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/commands))

(defvar *async-shell-command-buffer-name* "*Async Shell Command*"
  "Name of the buffer that receives asynchronous shell command output.")

(defun search-filename-and-recenter (filename)
  "Search `filename` in this files listing, recenter the window on it"
  (move-to-beginning-of-buffer)
  (search-forward (current-point) filename)
  (window-recenter (current-window))
  (character-offset (current-point) (* -1 (length filename))))

(define-command directory-mode-update-buffer () ()
  (update-buffer (current-buffer)))

(define-command directory-mode-up-directory () ()
  (let ((dir (buffer-directory)))
    (switch-to-buffer
     (directory-buffer (uiop:pathname-parent-directory-pathname (buffer-directory))))
    (search-filename-and-recenter
     (concatenate
      'string
      (car
       (reverse
        (split-sequence:split-sequence
         (uiop:directory-separator-for-host)
         dir
         :remove-empty-subseqs t)))
      (string (uiop:directory-separator-for-host))))))

(define-command directory-mode-find-file () ()
  (open-selected-file :read-only nil :next-window nil))

(define-command directory-mode-read-file () ()
  (open-selected-file :read-only t :next-window nil))

(define-command directory-mode-find-file-next-window () ()
  (open-selected-file :read-only nil :next-window t))

(define-command directory-mode-next-line (p) (:universal)
  (line-offset (current-point) p))

(define-command directory-mode-previous-line (p) (:universal)
  (line-offset (current-point) (- p)))

(define-command directory-mode-mark-and-next-line () ()
  (set-mark (current-point) t)
  (directory-mode-next-line 1))

(define-command directory-mode-unmark-and-next-line () ()
  (set-mark (current-point) nil)
  (directory-mode-next-line 1))

(define-command directory-mode-unmark-and-previous-line () ()
  (directory-mode-previous-line 1)
  (set-mark (current-point) nil))

(define-command directory-mode-toggle-marks () ()
  (filter-marks (current-point)
                (lambda (p) (not (get-mark p)))))

(define-command directory-mode-unmark-all () ()
  (filter-marks (current-point) (constantly nil)))

(define-command directory-mode-mark-regexp (regex &optional arg) ((:string "Regex: ") :universal-nil)
  "Mark all files matching the regular expression REGEX.
With prefix argument ARG, unmark all those files."
  (let ((scanner (ppcre:create-scanner regex)))
    (filter-marks (current-point)
                  (lambda (p)
                    (if (ppcre:scan scanner (get-name p))
                        (not arg)
                        (get-mark p))))))

(define-command directory-mode-mark-directories (&optional arg) (:universal-nil)
  "Mark all directories in the current buffer except '..'.
With prefix argument ARG, unmark all those directories."
  (filter-marks (current-point)
                (lambda (p)
                  (line-start p)
                  (move-to-file-position p)
                  (if (eq 'directory-attribute (text-property-at p :attribute))
                      (not arg)
                      (get-mark p)))))

(define-command directory-mode-mark-links (&optional arg) (:universal-nil)
  "Mark all symbolic links in the current buffer.
With prefix argument ARG, unmark all those links."
  (filter-marks (current-point)
                (lambda (p)
                  (line-start p)
                  (move-to-file-position p)
                  (if (eq 'link-attribute (text-property-at p :attribute))
                      (not arg)
                      (get-mark p)))))

(define-command directory-mode-mark-suffix (suffix &optional arg) ((:string "Suffix: ") :universal-nil)
  "Mark all files with the given SUFFIX.
The provided SUFFIX is a string, and not a file extenion, meaning every file with
a name ending in SUFFIX will be marked.
With prefix argument ARG, unmark all those files."
  (filter-marks (current-point)
                (lambda (p)
                  (let ((name (get-name p)))
                    ;; Use < so exact matches are not marked
                    (if (and (< (length suffix) (length name))
                             (string= name suffix :start1 (- (length name) (length suffix))))
                        (not arg)
                        (get-mark p))))))

(define-command directory-mode-mark-extension (extension &optional arg) ((:string "Extension: ") :universal-nil)
  "Mark all files with the given EXTENSION.
A '.' is prepended to the EXTENSION if not present.
With prefix argument ARG, unmark all those files."
  ;; Support empty extension, which will mark all files ending with a '.'.
  (when (or (= 0 (length extension))
            (char/= (aref extension 0) #\.))
    (setf extension (concatenate 'string "." extension)))
  (directory-mode-mark-suffix extension arg))

(define-command directory-mode-next-mark (n) (:universal)
  "Move to the next Nth marked entry."
  (cond ((= 0 n)
         nil)
        ((< n 0)
         (directory-mode-previous-mark (- n)))
        (t (let* ((all-marks (delete-if (lambda (p)
                                          (point< p (current-point)))
                                        (marked-lines (current-point))))
                  (result (nth (- n 1) all-marks)))
             (if result
                 (progn
                   (move-point (current-point) result)
                   (move-to-file-position (current-point)))
                 (editor-error "No next mark"))))))

(define-command directory-mode-previous-mark (n) (:universal)
  "Move to the previous Nth marked entry."
  (cond ((= 0 n) nil)
        ((< n 0) (directory-mode-next-mark (- n)))
        (t (with-point ((point (current-point)))
             (line-start point)
             (let* ((all-marks (delete-if (lambda (p)
                                            (point>= p point))
                                          (marked-lines point)))
                    (result (last all-marks n)))
               (if (and result
                        (= n (length result)))
                   (progn
                     (move-point (current-point) (car result))
                     (move-to-file-position (current-point)))
                   (editor-error "No previous mark")))))))

(defun query-replace-marked-files (query-function)
  (destructuring-bind (before after)
      (lem/isearch:read-query-replace-args)
    (dolist (file (marked-files (current-point)))
      (find-file file)
      (buffer-start (current-point))
      (funcall query-function before after))))

(define-command directory-mode-query-replace () ()
  (query-replace-marked-files 'lem/isearch:query-replace))

(define-command directory-mode-query-replace-regexp () ()
  (query-replace-marked-files 'lem/isearch:query-replace-regexp))

(define-command directory-mode-query-replace-symbol () ()
  (query-replace-marked-files 'lem/isearch:query-replace-symbol))

(define-command directory-mode-delete-files () ()
  (let ((files (selected-files (current-point))))
    (when (prompt-for-y-or-n-p (format nil
                                       "Really delete files~%~{- ~A~%~}"
                                       (mapcar #'uiop:native-namestring files)))
      (dolist (file files)
        (delete-file* file))
      (update-all-buffers))))

(defun get-dest-directory ()
  (dolist (window (window-list) (buffer-directory))
    (when (and (not (eq window (current-window)))
               (eq 'directory-mode (buffer-major-mode (window-buffer window))))
      (return (buffer-directory (window-buffer window))))))

(define-command directory-mode-copy-files () ()
  (let ((dst-file (prompt-for-file "Destination Filename: " :directory (get-dest-directory)))
        (files (selected-files (current-point))))
    (copy-files files dst-file))
  (update-all-buffers))

(define-command directory-mode-rename-files () ()
  (let ((dst-file (prompt-for-file "Destination: " :directory (get-dest-directory)))
        (files (selected-files (current-point))))
    (when (and (< 1 (length files))
               (uiop:file-pathname-p dst-file))
      (editor-error "Please select a directory to move multiple files."))
    (rename-files files dst-file))
  (update-all-buffers))

(defun isolated-shell-wildcard-p (command wildcard)
  "Return T if COMMAND contains WILDCARD surrounded by whitespace."
  (ppcre:scan (format nil "(?:^|\\s)\\~C(?=\\s|$)" wildcard) command))

(defun isolated-shell-qmark-p (command)
  "Return T if COMMAND contains an isolated `?' or `` `?' ''."
  (or (isolated-shell-wildcard-p command #\?)
      (ppcre:scan "`\\?'" command)))

(defun replace-isolated-shell-wildcard (command wildcard replacement)
  "Replace the isolated WILDCARD marks in COMMAND with REPLACEMENT.
The whitespace surrounding each mark is preserved.  For `?', the
`` `?' '' form is replaced as well."
  (let ((regex (if (char= wildcard #\?)
                   "(^|\\s)\\?(?=\\s|$)|`\\?'"
                   "(^|\\s)\\*(?=\\s|$)")))
    (ppcre:regex-replace-all
     regex
     command
     (lambda (target-string start end match-start match-end reg-starts reg-ends)
       (declare (ignore start end match-start match-end))
       (if (aref reg-starts 0)
           (concatenate 'string
                        (subseq target-string (aref reg-starts 0) (aref reg-ends 0))
                        replacement)
           replacement)))))

(defun shell-command-file-argument (pathname directory)
  "Return PATHNAME as a shell-quoted argument relative to DIRECTORY."
  (let* ((relative-name (uiop:native-namestring
                         (uiop:enough-pathname pathname directory)))
         (name (if (uiop:string-prefix-p "-" relative-name)
                   (concatenate 'string "./" relative-name)
                   relative-name)))
    (uiop:escape-shell-token name)))

(defun make-async-shell-command-lines (command files directory)
  "Return the shell commands to run for FILES, following dired's rules.
If COMMAND contains an isolated `*', substitute the whole file list.
If it contains an isolated `?' or `` `?' '', substitute one file name
per command.  Otherwise append the file name to COMMAND."
  (let ((file-arguments
          (mapcar (lambda (file)
                    (shell-command-file-argument file directory))
                  files)))
    (cond
      ((isolated-shell-wildcard-p command #\*)
       (when (isolated-shell-qmark-p command)
         (editor-error "You can not combine `*' and `?' substitution marks"))
       (list (replace-isolated-shell-wildcard
              command
              #\*
              (format nil "~{~A~^ ~}" file-arguments))))
      ((isolated-shell-qmark-p command)
       (mapcar (lambda (file-argument)
                 (replace-isolated-shell-wildcard command #\? file-argument))
               file-arguments))
      (t
       (mapcar (lambda (file-argument)
                 (format nil "~A ~A" command file-argument))
               file-arguments)))))

(defun strip-async-shell-command-suffix (command)
  "Remove a trailing `&' or `;' from COMMAND.
Dired treats these as requests to run the command in the background,
which this command does regardless."
  (loop :for stripped := (ppcre:regex-replace "\\s*[;&]\\s*\\z" command "")
        :until (string= stripped command)
        :do (setf command stripped)
        :finally (return (string-trim '(#\space #\tab #\newline) command))))

(defun combine-async-shell-command-lines (commands)
  "Combine per-file COMMANDS so they run in parallel in one shell."
  (cond
    ((null commands) "")
    ((null (cdr commands)) (first commands))
    (t
     (let ((joined (format nil "~{~A~^ & ~}" commands)))
       #-windows (concatenate 'string joined " & wait")
       #+windows joined))))

(defun async-shell-command-output-buffer ()
  "Return the output buffer, clearing any previous contents."
  (let ((buffer (make-buffer *async-shell-command-buffer-name*
                             :read-only-p t
                             :enable-undo-p nil)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer))
    buffer))

(defun append-async-shell-command-output (buffer string)
  "Append STRING to the output buffer BUFFER."
  (unless (deleted-buffer-p buffer)
    (let ((*inhibit-read-only* t))
      (insert-string (buffer-end-point buffer) string))))

(defun collect-async-shell-command-output (process buffer)
  "Copy the output of PROCESS into BUFFER until PROCESS exits."
  (let ((stream (uiop:process-info-output process)))
    (unwind-protect
         (loop :for line := (read-line stream nil nil)
               :while line
               :do (let ((line line))
                     (send-event
                      (lambda ()
                        (append-async-shell-command-output
                         buffer
                         (format nil "~A~%" line))))))
      (ignore-errors (close stream))
      (let ((exit-code (uiop:wait-process process)))
        (when (and exit-code (not (eql 0 exit-code)))
          (send-event
           (lambda ()
             (message "Shell command exited with status ~A" exit-code))))))))

(defun launch-async-shell-command (command directory buffer)
  "Run COMMAND in DIRECTORY, appending its output to BUFFER."
  (let ((process (uiop:launch-program command
                                      :directory directory
                                      :output :stream
                                      :error-output :output)))
    (bt2:make-thread
     (lambda ()
       (collect-async-shell-command-output process buffer))
     :name "Lem async shell command")))

(define-command directory-mode-do-async-shell-command () ()
  "Run a shell command asynchronously on the marked files.

The command is expanded like Emacs's `dired-do-async-shell-command':

- If COMMAND contains `*' surrounded by whitespace, it is run once
  with the whole list of marked file names substituted for `*'.
- If COMMAND contains `?' surrounded by whitespace, or `` `?' '', it is
  run once per file with that file name substituted.
- Otherwise it is run once per file with the file name appended.

The command runs in the directory buffer's directory and its output is
shown in the `*Async Shell Command*' buffer.  The directory buffer is
not refreshed automatically; use \\[directory-mode-update-buffer] to
refresh it."
  (let ((files (selected-files (current-point)))
        (directory (buffer-directory)))
    (unless files
      (editor-error "No file on this line"))
    (let* ((command (prompt-for-string
                     (format nil "& on ~D file~:P: " (length files))
                     :history-symbol 'directory-mode-shell-command))
           (command (strip-async-shell-command-suffix command)))
      (unless (str:non-blank-string-p command)
        (editor-error "No shell command"))
      (let* ((buffer (async-shell-command-output-buffer))
             (commands (make-async-shell-command-lines command files directory)))
        (launch-async-shell-command
         (combine-async-shell-command-lines commands)
         directory
         buffer)
        (pop-to-buffer buffer)))))

(defun move-to-file-position (point)
  (with-point ((limit point))
    (line-end limit)
    (next-single-property-change point :file limit)))

(defun replace-file-name (point string)
  (when (alexandria:emptyp string) (setf string " "))
  (line-start point)
  (move-to-file-position point)
  (character-at point 1)
  (let ((file (text-property-at point :file))
        (*inhibit-read-only* t))
    (with-point ((end point))
      (line-end end)
      (delete-between-points point end)
      (insert-string point string :file file))))

(defun prompt-for-rename-file (point)
  "Open an inlined prompt at point to rename the file."
  (let ((file (current-file point)))
    (save-excursion
      (move-point (current-point) point)
      (prompt-for-string
       "New name: "
       :initial-value (if file (file-namestring file) "")
       :test-function (lambda (string)
                        (not (alexandria:emptyp string)))
       :gravity :cursor
       :use-border nil))))

(define-command directory-mode-rename-file () ()
  "Edit the file name at point.
  Also rename the file's buffer, if it is open."
  (with-point ((point (current-point) :right-inserting))
    (move-to-file-position point)
    (alexandria:when-let (source-file (text-property-at point :file))
      (replace-file-name point "")
      (unwind-protect
           (let* ((old-file (merge-pathnames source-file
                                             (buffer-directory)))
                  (new-name (prompt-for-rename-file point))
                  (new-file (merge-pathnames new-name
                                             (buffer-directory (current-buffer)))))
             (when (probe-file new-file)
               (editor-error "The filename already exists."))
             (rename-file* source-file new-file)
             ;; Rename the old buffer.
             (alexandria:when-let (old-buffer (get-file-buffer old-file))
               (buffer-rename old-buffer new-name)
               (setf (buffer-filename old-buffer) new-file)))
        (directory-mode-update-buffer)))))

(defun sort-files-by (method &key reverse)
  "Sort files by the given method (:pathname, :name, :msize),
  and move the cursor on the current file's line."
  (let ((path (get-pathname (current-point))))
    (setf (buffer-value (current-buffer) :sort-method) method)
    (setf (buffer-value (current-buffer) :sort-reverse) reverse)
    (update-buffer (current-buffer) :sort-method method :sort-reverse reverse)
    ;; Follow file name.
    (when (and path (str:non-blank-string-p (file-namestring path)))
      (search-filename-and-recenter (display-name path)))))

(define-command directory-mode-sort-by-name () ()
  (sort-files-by :pathname))

(define-command directory-mode-sort-by-time () ()
  (sort-files-by :mtime))

(define-command directory-mode-sort-by-size () ()
  (sort-files-by :size))

(define-command directory-mode-sort-by-extension () ()
  (sort-files-by :extension))

(define-command directory-mode-sort-files () ()
  "Sort files: by name, by last modification time, then by size.

  Each new directory buffer first uses the default sort method (`lem/directory-mode:*default-sort-method*')"
  (cond
    ((eql (buffer-value (current-buffer) :sort-method) :mtime)
     (message "Sorting by size")
     (sort-files-by :size))
    ((eql (buffer-value (current-buffer) :sort-method) :size)
     (message "Sorting by name")
     (sort-files-by :pathname))
    (t
     (message "Sorting by last modification time")
     (sort-files-by :mtime))))

(define-command directory-mode-sort-reverse () ()
  "Reverse the current sort order."
  (let ((method (buffer-value (current-buffer) :sort-method))
        (reversed (buffer-value (current-buffer) :sort-reverse)))
    (sort-files-by method :reverse (not reversed))))

(define-command make-directory (filename) ((:new-file "Make directory: "))
  (setf filename (uiop:ensure-directory-pathname filename))
  (ensure-directories-exist filename)
  (update-all-buffers))

(define-command find-file-directory () ()
  "Open this file's directory and place point on the filename."
  (let ((fullpath (buffer-filename)))
    (cond
      ((mode-active-p (current-buffer) 'directory-mode)
       (directory-mode-up-directory))
      ((null fullpath)
       (switch-to-buffer (find-file-buffer (uiop:getcwd))))
      (t
       (switch-to-buffer
        (find-file-buffer (pathname (lem-core/commands/file::directory-for-file-or-lose (buffer-directory)))))
       (search-filename-and-recenter (display-name fullpath))))))

(define-command directory-mode-kill-lines () ()
  "Delete the marked lines from the directory-mode buffer.
This does not delete the marked entries, but only remove them from the buffer."
  (with-buffer-read-only (current-buffer) nil
    (let ((*inhibit-read-only* t)
          (marked-lines (marked-lines (current-point))))
      (save-excursion
        ;; Reverse the lines so killing is done from the end of the buffer
        (loop :for marked-line :in (nreverse marked-lines)
              :do (move-point (current-point) marked-line)
                  (kill-whole-line))))))

(defmethod execute :after ((mode directory-mode) command argument)
  (when (mode-active-p (current-buffer) 'directory-mode)
    (update-highlight-line-overlay (current-point))))
