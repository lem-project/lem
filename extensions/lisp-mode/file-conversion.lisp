(in-package :lem-lisp-mode/internal)

(defvar *file-conversion-map*
  '(("/nix/store/*-source-patched/" . "./"))
  "This variable is an alist for converting remote file names to local file names.
Uses include mapping files in docker to files in the local environment,
or mapping Nix store paths to local project directories.

Default: Maps Nix store source-patched paths to current project directory.

For Docker environment:
\(setf *file-conversion-map*
      `((\"/app/\" .
         ,(merge-pathnames \"common-lisp/my-project/\" (user-homedir-pathname)))))

For custom Nix patterns:
\(setf *file-conversion-map*
      '((\"/nix/store/*-alexandria-*/\" . \"~/.qlot/dists/quicklisp/software/alexandria-*/\")
        (\"/nix/store/*-bordeaux-threads-*/\" . \"~/.qlot/dists/quicklisp/software/bordeaux-threads-*/\")))

Glob pattern syntax:
  * - matches any characters except /
  ** - matches any characters including /
")

(defun has-glob-wildcard-p (pattern)
  "Check if PATTERN contains glob wildcards (* or **)."
  (or (search "*" pattern)))

(defun glob-pattern-to-regex (pattern)
  "Convert a glob pattern to a regular expression.
   * matches any characters except /
   ** matches any characters including /"
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (vector-push-extend #\^ result)
    (loop :with i := 0
          :with len := (length pattern)
          :while (< i len)
          :for char := (char pattern i)
          :do (cond
                ;; Handle ** (matches anything including /)
                ((and (char= char #\*)
                      (< (1+ i) len)
                      (char= (char pattern (1+ i)) #\*))
                 (loop :for c :across ".*" :do (vector-push-extend c result))
                 (incf i 2))
                ;; Handle * (matches anything except /)
                ((char= char #\*)
                 (loop :for c :across "[^/]*" :do (vector-push-extend c result))
                 (incf i))
                ;; Escape regex special characters
                ((find char "\\^$.|?+()[]{}")
                 (vector-push-extend #\\ result)
                 (vector-push-extend char result)
                 (incf i))
                ;; Normal character
                (t
                 (vector-push-extend char result)
                 (incf i))))
    result))

(defun match-glob-pattern (pattern path)
  "Match PATH against glob PATTERN.
   Returns (values matched-p matched-prefix rest-of-path) if matched."
  (if (has-glob-wildcard-p pattern)
      (let* ((regex (glob-pattern-to-regex pattern))
             (scanner (ppcre:create-scanner regex)))
        (multiple-value-bind (start end)
            (ppcre:scan scanner path)
          (when (and start (zerop start))
            (values t (subseq path 0 end) (subseq path end)))))
      ;; No wildcards - use simple prefix match
      (when (alexandria:starts-with-subseq pattern path)
        (values t pattern (subseq path (length pattern))))))

(defun expand-path (pattern)
  "Expand PATTERN to an absolute path.
   - ~ or ~/ expands to home directory
   - ./ or relative paths expand relative to current directory
   - Absolute paths are returned as-is"
  (cond
    ;; Home directory expansion
    ((alexandria:starts-with-subseq "~/" pattern)
     (uiop:native-namestring
      (merge-pathnames (subseq pattern 2) (user-homedir-pathname))))
    ((string= pattern "~")
     (uiop:native-namestring (user-homedir-pathname)))
    ;; Absolute path
    ((alexandria:starts-with #\/ pattern)
     pattern)
    ;; Relative path (including ./)
    (t
     (uiop:native-namestring
      (merge-pathnames pattern (uiop:getcwd))))))

(defun resolve-glob-directory (pattern)
  "Resolve a glob pattern to an actual directory path.
   Returns the first matching directory, or the expanded pattern if no wildcards."
  (let ((expanded (expand-path pattern)))
    (if (has-glob-wildcard-p expanded)
        (let* (;; Find the non-wildcard prefix
               (wildcard-pos (or (position #\* expanded) (length expanded)))
               (prefix-end (1+ (or (position #\/ expanded :end wildcard-pos :from-end t) 0)))
               (base-dir (subseq expanded 0 prefix-end))
               (glob-suffix (subseq expanded prefix-end)))
          (when (uiop:directory-exists-p base-dir)
            (let ((matches (directory (merge-pathnames (concatenate 'string glob-suffix "/")
                                                       base-dir)
                                      :resolve-symlinks nil)))
              (when matches
                (namestring (first matches))))))
        ;; No wildcards - return expanded pattern if directory exists
        (when (uiop:directory-exists-p expanded)
          expanded))))

(defun convert-with-glob-pattern (filename remote-pattern local-pattern)
  "Convert FILENAME using glob patterns.
   Returns converted path or nil if no match."
  (multiple-value-bind (matched-p matched-prefix rest-path)
      (match-glob-pattern remote-pattern filename)
    (declare (ignore matched-prefix))
    (when matched-p
      (let ((resolved-local (resolve-glob-directory local-pattern)))
        (when resolved-local
          (concatenate 'string
                       (string-right-trim "/" resolved-local)
                       (if (alexandria:starts-with #\/ rest-path)
                           rest-path
                           (concatenate 'string "/" rest-path))))))))

(defun convert-remote-to-local-file (filename)
  "Convert remote FILENAME to local path using *file-conversion-map*.
   Supports both simple prefix matching and glob patterns."
  (loop :for (remote-pattern . local-pattern) :in *file-conversion-map*
        :do (if (has-glob-wildcard-p remote-pattern)
                ;; Glob pattern matching
                (alexandria:when-let ((result (convert-with-glob-pattern
                                               filename remote-pattern local-pattern)))
                  (return result))
                ;; Simple prefix matching (backward compatible)
                (when (alexandria:starts-with-subseq remote-pattern filename)
                  (return (concatenate 'string local-pattern
                                       (subseq filename (length remote-pattern))))))
        :finally (return filename)))

(defun convert-local-to-remote-file (filename)
  "Convert local FILENAME to remote path using *file-conversion-map*.
   Note: Glob patterns in local-pattern are not supported for reverse conversion."
  (loop :for (remote-pattern . local-pattern) :in *file-conversion-map*
        :do (unless (has-glob-wildcard-p local-pattern)
              (when (alexandria:starts-with-subseq local-pattern filename)
                (return (concatenate 'string remote-pattern
                                     (subseq filename (length local-pattern))))))
        :finally (return filename)))
