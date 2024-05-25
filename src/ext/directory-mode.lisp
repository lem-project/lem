(defpackage :lem/directory-mode
  (:use :cl :lem)
  #+sbcl
  (:lock t)
  (:export
   :*default-sort-method*))
(in-package :lem/directory-mode)

(deftype sort-method ()
  '(member :pathname :mtime :size))

(declaim (type (sort-method) *default-sort-method*))
(defvar *default-sort-method* :pathname
  "Default method to sort files when opening a directory.

  A keyword, one of :pathname (sort by file name), :mtime (last modification time) and :size.")

(define-attribute current-directory-attribute
  (t :bold t))

(define-attribute file-size-attribute
  (t))

(define-attribute file-date-attribute
  (t))

(define-attribute file-attribute
  (t))

(define-attribute directory-attribute
  (t :foreground :base0D :bold t))

(define-attribute link-attribute
  (t :foreground :base0B :bold t))

(define-major-mode directory-mode ()
    (:name "Directory"
     :keymap *directory-mode-keymap*)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil))

(define-key *global-keymap* "C-x C-j" 'find-file-directory)

(define-key *directory-mode-keymap* "q" 'quit-active-window)
(define-key *directory-mode-keymap* "M-q" 'quit-active-window)
(define-key *directory-mode-keymap* "g" 'directory-mode-update-buffer)
(define-key *directory-mode-keymap* "^" 'directory-mode-up-directory)
(define-key *directory-mode-keymap* "Return" 'directory-mode-find-file)
(define-key *directory-mode-keymap* "Space" 'directory-mode-read-file)
(define-key *directory-mode-keymap* "o" 'directory-mode-find-file-next-window)
(define-key *directory-mode-keymap* "n" 'directory-mode-next-line)
(define-key *directory-mode-keymap* "p" 'directory-mode-previous-line)
(define-key *directory-mode-keymap* "M-}" 'directory-mode-next-mark)
(define-key *directory-mode-keymap* "M-{" 'directory-mode-previous-mark)
(define-key *directory-mode-keymap* "m" 'directory-mode-mark-and-next-line)
(define-key *directory-mode-keymap* "u" 'directory-mode-unmark-and-next-line)
(define-key *directory-mode-keymap* "U" 'directory-mode-unmark-and-previous-line)
(define-key *directory-mode-keymap* "t" 'directory-mode-toggle-marks)
(define-key *directory-mode-keymap* "* !" 'directory-mode-unmark-all)
(define-key *directory-mode-keymap* "* %" 'directory-mode-mark-regexp)
(define-key *directory-mode-keymap* "* /" 'directory-mode-mark-directories)
(define-key *directory-mode-keymap* "* @" 'directory-mode-mark-links)
(define-key *directory-mode-keymap* "* C-n" 'directory-mode-next-mark)
(define-key *directory-mode-keymap* "* C-p" 'directory-mode-previous-mark)
(define-key *directory-mode-keymap* "Q" 'directory-mode-query-replace)
(define-key *directory-mode-keymap* "D" 'directory-mode-delete-files)
(define-key *directory-mode-keymap* "C" 'directory-mode-copy-files)
(define-key *directory-mode-keymap* "R" 'directory-mode-rename-files)
(define-key *directory-mode-keymap* "r" 'directory-mode-rename-file)
(define-key *directory-mode-keymap* "s" 'directory-mode-sort-files)
(define-key *directory-mode-keymap* "+" 'make-directory)
(define-key *directory-mode-keymap* "C-k" 'directory-mode-kill-lines)

(defun run-command (command)
  (when (consp command)
    (setf command (mapcar #'princ-to-string command)))
  (let ((error-string
         (with-output-to-string (error-output)
           (uiop:run-program command
                             :ignore-error-status t
                             :error-output error-output))))
    (when (string/= error-string "")
      (editor-error "~A" error-string))))

(defun remove-line-overlay-when-buffer-change (point arg)
  (declare (ignore arg))
  (alexandria:when-let (ov (buffer-value (point-buffer point) 'line-overlay))
    (setf (buffer-value (point-buffer point) 'line-overlay) nil)
    (delete-overlay ov)))

(defun update-line (point)
  (with-point ((start point)
               (end point))
    (back-to-indentation start)
    (line-end end)
    (let ((ov (buffer-value point 'line-overlay)))
      (cond (ov
             (move-point (overlay-start ov) start)
             (move-point (overlay-end ov) end))
            (t
             (add-hook (variable-value 'before-change-functions :buffer (point-buffer point))
                       'remove-line-overlay-when-buffer-change)
             (setf ov (make-overlay start end 'region))
             (setf (buffer-value point 'line-overlay) ov))))))

(defun move-to-start-line (point)
  (move-to-line point 3))

(defun get-line-property (p key)
  (with-point ((p p))
    (text-property-at (line-start p) key)))

(defun get-pathname (point)
  (get-line-property point 'pathname))

(defun get-name (point)
  (get-line-property point 'name))

(defun get-mark (p)
  (with-point ((p p))
    (eql #\* (character-at (line-start p) 1))))

(defun set-mark (p mark)
  (with-buffer-read-only (point-buffer p) nil
    (let ((*inhibit-read-only* t))
      (with-point ((p p))
        (let ((pathname (get-line-property p 'pathname)))
          (when (and pathname (not (uiop:pathname-equal
                                    pathname
                                    (uiop:pathname-parent-directory-pathname
                                     (buffer-directory (point-buffer p))))))
            (character-offset (line-start p) 1)
            (delete-character p 1)
            (insert-character p (if mark #\* #\space))))))))

(defun iter-marks (p function)
  (with-point ((p p))
    (move-to-start-line p)
    (loop
     (funcall function p)
     (unless (line-offset p 1) (return)))))


(defun marked-lines (p)
  (with-point ((p p))
    (let ((points '()))
      (iter-marks p
                  (lambda (p)
                    (when (get-mark p)
                      (push (copy-point p :temporary) points))))
      (nreverse points))))

(defun marked-files (p)
  (mapcar 'get-pathname (marked-lines p)))

(defun filter-marks (p function)
  (iter-marks p
              (lambda (p)
                (set-mark p (funcall function p)))))

(defun current-file (p)
  (alexandria:when-let (pathname (get-pathname p))
    pathname))

(defun selected-files (p)
  (or (marked-files p)
      (uiop:ensure-list (current-file p))))

(defun process-current-line-pathname (function)
  (alexandria:when-let (pathname (get-pathname (current-point)))
    (funcall function pathname)))

(defun symbolic-link-p (pathname)
  (not (uiop:pathname-equal pathname (probe-file pathname))))

(defun get-file-attribute (pathname)
  (cond ((symbolic-link-p pathname)
         'link-attribute)
        ((uiop:directory-pathname-p pathname)
         'directory-attribute)
        (t
         'file-attribute)))

(defun human-readable-file-size (size)
  (loop :for sign :in '(#\Y #\Z #\E #\P #\T #\G #\M #\k)
        :for val := (expt 1024 8) :then (/ val 1024)
        :do (when (>= size val)
              (return (format nil "~D~C" (1+ (floor size val)) sign)))
        :finally (return (princ-to-string size))))

(defun insert-icon (point pathname)
  (cond ((uiop:directory-pathname-p pathname)
         (insert-string point (icon-string "folder")))
        ((let ((string (icon-string-by-ext (pathname-type pathname))))
           (when string
             (insert-string point string)
             t)))
        (t
         (let ((string (icon-string-by-ext "txt")))
           (when string
             (insert-string point string)
             t)))))

(defun insert-pathname (point pathname directory &optional content)
  (let ((file-size (handler-case (file-size pathname)
                     (error ()
                       (return-from insert-pathname)))))
    (with-point ((start point))
      (let ((name (or content (namestring (enough-namestring pathname directory)))))
        (insert-string point "  " 'pathname pathname 'name name)
        (insert-string point
                       (format nil " ~5@A "
                               (if file-size (human-readable-file-size file-size) ""))
                       :attribute 'file-size-attribute)
        (multiple-value-bind (second minute hour day month year week)
            (let ((date (file-write-date pathname)))
              (if date
                  (decode-universal-time date)
                  (values 0 0 0 0 0 0 nil)))
          (insert-string point
                         (format nil "~4,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D ~A "
                                 year month day hour minute second
                                 (if week (aref #("Mon" "Tue" "Wed" "Thr" "Fri" "Sat" "Sun") week)
                                     "   "))
                         :attribute 'file-date-attribute))
        (unless (string= name "..")
          (insert-icon point name))
        (insert-string point
                       name
                       :attribute (get-file-attribute pathname)
                       :file pathname)
        (when (symbolic-link-p pathname)
          (insert-string point (format nil " -> ~A" (probe-file pathname))))
        (back-to-indentation start)
        (lem/button:apply-button-between-points
         start point
         (lambda ()
           (lem/button:with-context ()
             (directory-mode-find-file))))
        (insert-character point #\newline)
        (put-text-property start point :read-only t)))))

(defun insert-directories-and-files (point
                                     directory
                                     &key (sort-method *default-sort-method*)
                                          (without-parent-directory t))
  (unless without-parent-directory
    (alexandria:when-let (pathname (probe-file (merge-pathnames "../" directory)))
      (insert-pathname point pathname directory "..")))
  (dolist (pathname (list-directory directory :sort-method sort-method))
    (insert-pathname point pathname directory)))

(defun update (buffer &key (sort-method *default-sort-method*))
  "Update this directory buffer content."
  (with-buffer-read-only buffer nil
    (let ((*inhibit-read-only* t))
      (let* ((directory (buffer-directory buffer))
             (p (buffer-point buffer))
             (line-number (line-number-at-point p)))
        (erase-buffer buffer)
        (buffer-start p)
        (insert-string p (format nil "~A~2%" directory) :attribute 'current-directory-attribute)
        (insert-directories-and-files p directory
                                      :sort-method sort-method
                                      :without-parent-directory nil)
        (move-to-line p line-number)))))

(defun update-all ()
  (dolist (buffer (buffer-list))
    (when (eq 'directory-mode (buffer-major-mode buffer))
      (update buffer))))

(defun create-directory-buffer (name filename)
  (let ((buffer (make-buffer name :enable-undo-p nil :read-only-p t)))
    (change-buffer-mode buffer 'directory-mode)
    (setf (buffer-directory buffer) filename)
    (update buffer)
    (move-to-start-line (buffer-point buffer))
    buffer))

(defun directory-buffer (filename)
  (setf filename (uiop:directory-exists-p
                  (expand-file-name (namestring filename)
                                    (buffer-directory))))
  (let* ((name (pathname-directory-last-name filename))
         (buffer (get-buffer name)))
    (cond ((and buffer (not (uiop:pathname-equal filename (buffer-directory buffer))))
           (create-directory-buffer (unique-buffer-name name) filename))
          ((null buffer)
           (create-directory-buffer name filename))
          (t
           buffer))))

(defun delete-file* (file)
  #+windows
  (if (uiop:directory-pathname-p file)
      (sb-ext:delete-directory file :recursive t)
      (delete-file file))
  #-windows
  (if (and (not (string= (namestring file)
                         (namestring (uiop:resolve-symlinks file)))))
      (and (prompt-for-y-or-n-p
             (format nil "It appears that ~a is a symlink, delete it?" file))
           (run-command `("unlink" ,(string-right-trim
                                     (string
                                      (uiop:directory-separator-for-host))
                                     (namestring file)))))
      (run-command `("rm" "-fr" ,file))))

(defun subdirectory-p (to-pathname from-pathname)
  (let ((to-dir (pathname-directory to-pathname))
        (from-dir (pathname-directory from-pathname)))
    (assert (eq :absolute (car to-dir)))
    (assert (eq :absolute (car from-dir)))
    (and (<= (length from-dir)
             (length to-dir))
         (loop
          :for from-elt :in (cdr from-dir)
          :for to-elt :in (cdr to-dir)
          :when (not (equal from-elt to-elt))
          :do (return nil)
          :finally (return t)))))

(defun pathname-directory-last-name (pathname)
  (enough-namestring pathname (uiop:pathname-parent-directory-pathname pathname)))

(defvar *rename-p* nil)

(defun copy-directory (src dst)
  (setf dst (uiop:ensure-directory-pathname dst))
  (let ((dst (if (probe-file dst)
                 (merge-pathnames (pathname-directory-last-name src)
                                  dst)
                 dst)))
    (when (subdirectory-p dst src)
      (editor-error "Cannot copy `~A' into its subdirectory `~A'" src dst))
    ;(format t "mkdir ~A~%" dst)
    (let ((dst (ensure-directories-exist dst)))
      (dolist (file (list-directory src))
        (copy-file file dst)))
    (when *rename-p* (uiop:delete-empty-directory src))))

(defun copy-file (src dst)
  (if (uiop:directory-pathname-p src)
      (copy-directory src dst)
      (let ((probe-dst (probe-file dst)))
        (cond ((and probe-dst (uiop:directory-pathname-p probe-dst))
               (copy-file src (merge-pathnames (file-namestring src) probe-dst)))
              (t
               ;(format t "copy ~A -> ~A~%" src dst)
               (if *rename-p*
                   (rename-file src dst)
                   (uiop:copy-file src dst)))))))

(defun copy-file* (src dst)
  #+windows
  (copy-file src dst)
  #-windows
  (run-command `("cp" "-R" ,src ,dst)))

(defun rename-file* (src dst)
  #+windows
  (let ((*rename-p* t))
    (copy-file src dst))
  #-windows
  (run-command `("mv" ,src ,dst)))

(defun copy-or-rename-file (src dst)
  #+windows
  (copy-file src dst)
  #-windows
  (if *rename-p*
      (run-command `("mv" ,src ,dst))
      (run-command `("cp" "-R" ,src ,dst))))

(defun check-copy-files (src-files dst)
  (let ((n (length src-files)))
    (cond ((or (and (< 1 n) (uiop:file-exists-p dst))
               (and (= 1 n) (uiop:directory-pathname-p (first src-files))
                    (uiop:file-exists-p dst)))
           (editor-error "Target must be a directory"))
          ((and (< 1 n) (not (uiop:directory-exists-p dst)))
           (editor-error "No such file or directory")))))

(defun copy-files (src-files dst-file)
  (check-copy-files src-files dst-file)
  (dolist (file src-files)
    (copy-or-rename-file file dst-file)))

(defun rename-files (src-files dst-file)
  (let ((*rename-p* t))
    (dolist (file src-files)
      (copy-or-rename-file file dst-file))))

(define-command directory-mode-update-buffer () ()
  (update (current-buffer)))

(define-command directory-mode-up-directory () ()
  (switch-to-buffer
   (directory-buffer
    (uiop:pathname-parent-directory-pathname
     (buffer-directory)))))

(define-command directory-mode-find-file () ()
  (process-current-line-pathname 'find-file))

(define-command directory-mode-read-file () ()
  (process-current-line-pathname 'read-file))

(define-command directory-mode-find-file-next-window () ()
  (process-current-line-pathname (lambda (pathname)
                                   (let ((buffer (execute-find-file *find-file-executor*
                                                                    (get-file-mode pathname)
                                                                    pathname)))
                                     (switch-to-window
                                           (pop-to-buffer buffer))))))

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
    (when (prompt-for-y-or-n-p (format nil "Really delete files~%~{- ~A~%~}" files))
      (dolist (file files)
        (delete-file* file))
      (update-all))))

(defun get-dest-directory ()
  (dolist (window (window-list) (buffer-directory))
    (when (and (not (eq window (current-window)))
               (eq 'directory-mode (buffer-major-mode (window-buffer window))))
      (return (buffer-directory (window-buffer window))))))

(define-command directory-mode-copy-files () ()
  (let ((dst-file (prompt-for-file "Destination Filename: " :directory (get-dest-directory)))
        (files (selected-files (current-point))))
    (copy-files files dst-file))
  (update-all))

(define-command directory-mode-rename-files () ()
  (let ((dst-file (prompt-for-file "Destination Filename: " :directory (get-dest-directory))))
    (rename-files (selected-files (current-point)) dst-file))
  (update-all))

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
  (let ((file (current-file point)))
    (save-excursion
      (move-point (current-point) point)
      (prompt-for-string
       ""
       :initial-value (if file (file-namestring file) "")
       :test-function (lambda (string)
                        (not (alexandria:emptyp string)))
       :gravity :cursor
       :use-border nil))))

(define-command directory-mode-rename-file () ()
  (with-point ((point (current-point) :right-inserting))
    (move-to-file-position point)
    (alexandria:when-let (source-file (text-property-at point :file))
      (replace-file-name point "")
      (unwind-protect
           (let* ((new-file (merge-pathnames (prompt-for-rename-file point)
                                             (buffer-directory (current-buffer)))))
             (when (probe-file new-file)
               (editor-error "The filename already exists."))
             (rename-file* source-file new-file))
        (directory-mode-update-buffer)))))

(defun search-filename-and-recenter (filename)
  "Search `filename` in this files listing, recenter the window on it"
  (move-to-beginning-of-buffer)
  (search-forward (current-point) filename)
  (window-recenter (current-window))
  (character-offset (current-point) (* -1 (length filename))))

(define-command directory-mode-sort-files () ()
  "Sort files: by name, by last modification time, then by size.

  Each new directory buffer first uses the default sort method (`lem/directory-mode:*default-sort-method*')"
  (let ((path (get-pathname (current-point))))
    (cond
      ;; mtime -> size
      ((eql (buffer-value (current-buffer) :sort-method) :mtime)
       (message "Sorting by size")
       (setf (buffer-value (current-buffer) :sort-method) :size)
       (update (current-buffer) :sort-method :size))
      ;; size -> pathname
      ((eql (buffer-value (current-buffer) :sort-method) :size)
       (message "Sorting by name")
       (setf (buffer-value (current-buffer) :sort-method) :pathname)
       (update (current-buffer) :sort-method :pathname))
      (t
       ;; At first call, the buffer's sort-method is not set.
       (message "Sorting by last modification time")
       (setf (buffer-value (current-buffer) :sort-method) :mtime)
       (update (current-buffer) :sort-method :mtime)))

    ;; Follow file name.
    (when (and path (str:non-blank-string-p (file-namestring path)))
      (search-filename-and-recenter (file-namestring path)))))

(define-command make-directory (filename) ((:new-file "Make directory: "))
  (setf filename (uiop:ensure-directory-pathname filename))
  (ensure-directories-exist filename)
  (update-all))

(define-command find-file-directory () ()
  "Open this file's directory and place point on the filename."
  (let ((fullpath (buffer-filename)))
    (cond
      ((null fullpath)
       (message "No file at point"))
      (t
       (switch-to-buffer
        (find-file-buffer (lem-core/commands/file::directory-for-file-or-lose (buffer-directory))))
       (let ((filename (file-namestring fullpath)))
         (search-filename-and-recenter (file-namestring filename)))))))

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

(setf *find-directory-function* 'directory-buffer)

(defmethod execute :after ((mode directory-mode) command argument)
  (when (mode-active-p (current-buffer) 'directory-mode)
    (update-line (current-point))))
