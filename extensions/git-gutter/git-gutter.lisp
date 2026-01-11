(defpackage :lem-git-gutter
  (:use :cl :lem)
  (:local-nicknames (:diff-parser :lem-git-gutter/diff-parser)
                    (:dir-mode :lem/directory-mode))
  (:import-from :lem/directory-mode
                :*file-entry-inserters*
                :item-pathname
                :item-directory
                :update-buffer
                :directory-mode)
  (:export :git-gutter-mode
           :git-gutter-set-ref
           :git-gutter-refresh
           :git-gutter-toggle-line-highlight
           :git-gutter-refresh-directory-status
           :*git-gutter-ref*
           :*git-gutter-highlight-line*
           :*git-gutter-update-delay*
           :*directory-status-cache-ttl*
           ;; For testing
           :parse-git-status-porcelain
           :parse-git-diff-name-status
           :status-to-display))
(in-package :lem-git-gutter)

;;; Configuration

(defvar *git-gutter-ref* "HEAD"
  "The git ref to compare against. Defaults to HEAD.")

(defvar *git-gutter-highlight-line* nil
  "When non-nil, highlight the entire line for added/modified lines.")

(defvar *git-gutter-update-delay* 300
  "Delay in milliseconds before updating gutter after edit.")

(defvar *directory-status-cache-ttl* 5000
  "Cache time-to-live in milliseconds for directory git status.")

;;; Attributes

(define-attribute git-gutter-added-attribute
  (t :foreground "green"))

(define-attribute git-gutter-added-line-attribute
  (t :background "#1a3d1a"))

(define-attribute git-gutter-modified-attribute
  (t :foreground "yellow"))

(define-attribute git-gutter-modified-line-attribute
  (t :background "#3d3d1a"))

(define-attribute git-gutter-deleted-attribute
  (t :foreground "red"))

(define-attribute git-gutter-deleted-line-attribute
  (t :background "#3d1a1a"))

;;; Git Operations

(defun run-git (args)
  "Run git command with args and return output string."
  (uiop:run-program (cons "git" args)
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun find-git-root (directory)
  "Find the git root directory starting from directory, walking up the tree."
  (loop :for dir := (uiop:truename* directory)
          :then (uiop:pathname-parent-directory-pathname dir)
        :while dir
        :when (uiop:directory-exists-p (merge-pathnames ".git/" dir))
          :return dir
        :when (equal dir (uiop:pathname-parent-directory-pathname dir))
          :return nil))

(defun get-git-diff-for-buffer (buffer ref)
  "Get git diff for a buffer's file (on disk).
   Returns the diff output string or nil."
  (alexandria:when-let ((filepath (buffer-filename buffer)))
    (let ((directory (or (buffer-directory buffer)
                         (uiop:pathname-directory-pathname filepath))))
      (alexandria:when-let ((git-root (find-git-root directory)))
        (uiop:with-current-directory (git-root)
          (let ((relative-path (enough-namestring filepath git-root)))
            (run-git (list "diff" "--no-color" "-U0" ref "--" relative-path))))))))

(defun get-original-content (ref relative-path)
  "Get the original file content from git ref.
   Returns the content string or nil if not found."
  (let ((result (run-git (list "show" (format nil "~A:~A" ref relative-path)))))
    (when (and result (plusp (length result)))
      result)))

(defun get-git-diff-for-unsaved-buffer (buffer ref)
  "Get git diff for unsaved buffer content using temp files.
   Returns the diff output string or nil."
  (alexandria:when-let ((filepath (buffer-filename buffer)))
    (let ((directory (or (buffer-directory buffer)
                         (uiop:pathname-directory-pathname filepath))))
      (alexandria:when-let ((git-root (find-git-root directory)))
        (uiop:with-current-directory (git-root)
          (let* ((relative-path (enough-namestring filepath git-root))
                 (original-content (get-original-content ref relative-path)))
            (when original-content
              (uiop:with-temporary-file (:pathname original-path :stream original-stream
                                         :direction :output
                                         :element-type 'character)
                (write-string original-content original-stream)
                (finish-output original-stream)
                (close original-stream)
                (uiop:with-temporary-file (:pathname buffer-path :stream buffer-stream
                                           :direction :output
                                           :element-type 'character)
                  (write-string (buffer-text buffer) buffer-stream)
                  (finish-output buffer-stream)
                  (close buffer-stream)
                  (run-git (list "diff" "--no-color" "-U0" "--no-index"
                                 (namestring original-path)
                                 (namestring buffer-path))))))))))))

;;; Buffer State Management

(defun buffer-git-gutter-changes (buffer)
  "Get the git gutter changes hash-table for buffer."
  (buffer-value buffer 'git-gutter-changes))

(defun (setf buffer-git-gutter-changes) (changes buffer)
  "Set the git gutter changes hash-table for buffer."
  (setf (buffer-value buffer 'git-gutter-changes) changes))

(defun buffer-git-gutter-overlays (buffer)
  "Get the git gutter overlays list for buffer."
  (buffer-value buffer 'git-gutter-overlays))

(defun (setf buffer-git-gutter-overlays) (overlays buffer)
  "Set the git gutter overlays list for buffer."
  (setf (buffer-value buffer 'git-gutter-overlays) overlays))

(defun clear-git-gutter-overlays (buffer)
  "Remove all git gutter overlays from buffer."
  (dolist (overlay (buffer-git-gutter-overlays buffer))
    (delete-overlay overlay))
  (setf (buffer-git-gutter-overlays buffer) nil))

(defun buffer-git-gutter-timer (buffer)
  "Get the git gutter update timer for buffer."
  (buffer-value buffer 'git-gutter-timer))

(defun (setf buffer-git-gutter-timer) (timer buffer)
  "Set the git gutter update timer for buffer."
  (setf (buffer-value buffer 'git-gutter-timer) timer))

(defun cancel-buffer-git-gutter-timer (buffer)
  "Cancel pending update timer for buffer."
  (alexandria:when-let ((timer (buffer-git-gutter-timer buffer)))
    (stop-timer timer)
    (setf (buffer-git-gutter-timer buffer) nil)))

(defun cancel-all-git-gutter-timers ()
  "Cancel all pending git gutter update timers."
  (dolist (buffer (buffer-list))
    (cancel-buffer-git-gutter-timer buffer)))

(defun change-type-to-line-attribute (change-type)
  "Return the line attribute for a change type.
   Returns nil for :deleted since we don't highlight the entire line."
  (case change-type
    (:added 'git-gutter-added-line-attribute)
    (:modified 'git-gutter-modified-line-attribute)
    (:deleted nil)  ; No line highlight for deletion marker
    (otherwise nil)))

(defun create-line-overlays (buffer changes)
  "Create overlays for changed lines when *git-gutter-highlight-line* is non-nil."
  (clear-git-gutter-overlays buffer)
  (when (and *git-gutter-highlight-line* changes)
    (let ((overlays nil))
      (with-point ((point (buffer-point buffer)))
        (buffer-start point)
        (maphash (lambda (line-number change-type)
                   (alexandria:when-let ((attr (change-type-to-line-attribute change-type)))
                     (move-to-line point line-number)
                     (push (make-line-overlay point attr) overlays)))
                 changes))
      (setf (buffer-git-gutter-overlays buffer) overlays))))

(defun update-git-gutter-for-buffer (buffer)
  "Update the git gutter diff information for buffer.
   Uses unsaved buffer content if buffer is modified."
  (let ((diff-output (if (buffer-modified-p buffer)
                         (get-git-diff-for-unsaved-buffer buffer *git-gutter-ref*)
                         (get-git-diff-for-buffer buffer *git-gutter-ref*))))
    (if (and diff-output (plusp (length diff-output)))
        (let ((changes (diff-parser:parse-git-diff diff-output)))
          (setf (buffer-git-gutter-changes buffer) changes)
          (create-line-overlays buffer changes))
        (progn
          (setf (buffer-git-gutter-changes buffer) nil)
          (clear-git-gutter-overlays buffer)))))

(defun update-all-buffers ()
  "Update git gutter for all file-backed buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-filename buffer)
      (update-git-gutter-for-buffer buffer))))

(defun clear-all-buffers ()
  "Clear git gutter state from all buffers."
  (dolist (buffer (buffer-list))
    (setf (buffer-git-gutter-changes buffer) nil)
    (clear-git-gutter-overlays buffer)))

;;; Change Detection

(defun git-gutter-on-change (start end old-len)
  "Called after buffer modification. Schedules debounced update."
  (declare (ignore end old-len))
  (let* ((buffer (point-buffer start))
         (existing-timer (buffer-git-gutter-timer buffer)))
    (when (buffer-filename buffer)
      ;; Cancel existing timer (debounce)
      (when existing-timer
        (stop-timer existing-timer))
      ;; Schedule new update
      (setf (buffer-git-gutter-timer buffer)
            (start-timer
             (make-idle-timer
              (lambda () (update-git-gutter-for-buffer buffer))
              :name "git-gutter-update")
             *git-gutter-update-delay*
             :repeat nil)))))

;;; Mode Definition

(defun git-gutter-after-save (buffer)
  "Hook function called after saving a buffer."
  (when (buffer-filename buffer)
    (update-git-gutter-for-buffer buffer)))

(defun add-directory-mode-inserter ()
  "Add git status inserter to directory-mode."
  (unless (member #'insert-git-status *file-entry-inserters*)
    (push #'insert-git-status *file-entry-inserters*)))

(defun remove-directory-mode-inserter ()
  "Remove git status inserter from directory-mode."
  (setf *file-entry-inserters*
        (remove #'insert-git-status *file-entry-inserters*)))

(defun update-all-directory-buffers ()
  "Update all directory-mode buffers to reflect git status."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-major-mode buffer) 'lem/directory-mode:directory-mode)
      (update-buffer buffer))))

(defun git-gutter-find-file (buffer)
  "Hook function called when a file is opened."
  (when (buffer-filename buffer)
    (update-git-gutter-for-buffer buffer)))

(defun enable-hook ()
  "Called when git-gutter-mode is enabled."
  (update-all-buffers)
  (add-hook *find-file-hook* 'git-gutter-find-file)
  (add-hook (variable-value 'after-save-hook :global t)
            'git-gutter-after-save)
  (add-hook (variable-value 'after-change-functions :global t)
            'git-gutter-on-change)
  ;; Add directory-mode support
  (add-directory-mode-inserter)
  (update-all-directory-buffers))

(defun disable-hook ()
  "Called when git-gutter-mode is disabled."
  (remove-hook *find-file-hook* 'git-gutter-find-file)
  (remove-hook (variable-value 'after-save-hook :global t)
               'git-gutter-after-save)
  (remove-hook (variable-value 'after-change-functions :global t)
               'git-gutter-on-change)
  (cancel-all-git-gutter-timers)
  (clear-all-buffers)
  ;; Remove directory-mode support and clear cache
  (remove-directory-mode-inserter)
  (clear-directory-status-cache))

(define-minor-mode git-gutter-mode
    (:name "GitGutter"
     :global t
     :enable-hook 'enable-hook
     :disable-hook 'disable-hook))

;;; Gutter Renderer

(defun make-gutter-content (change-type)
  "Create gutter content for the given change type."
  (case change-type
    (:added
     (lem/buffer/line:make-content
      :string "+"
      :attributes '((0 1 git-gutter-added-attribute))))
    (:modified
     (lem/buffer/line:make-content
      :string "~"
      :attributes '((0 1 git-gutter-modified-attribute))))
    (:deleted
     (lem/buffer/line:make-content
      :string "_"
      :attributes '((0 1 git-gutter-deleted-attribute))))
    (otherwise nil)))

(defmethod lem-core:compute-left-display-area-content
    ((mode git-gutter-mode) buffer point)
  "Compute the gutter content for git-gutter-mode."
  (let* ((changes (buffer-git-gutter-changes buffer))
         (line-number (line-number-at-point point))
         (change-type (and changes (gethash line-number changes))))
    (if change-type
        (make-gutter-content change-type)
        ;; No change: display space to maintain alignment
        (lem/buffer/line:make-content :string " "))))

;;; Commands

(define-command git-gutter-set-ref (ref) ((:string "Compare with ref: "))
  "Set the git ref to compare against (e.g., HEAD, HEAD~1, main, commit-sha)."
  (setf *git-gutter-ref* ref)
  (update-all-buffers)
  (message "Git gutter now comparing with: ~A" ref))

(define-command git-gutter-refresh () ()
  "Refresh git gutter for current buffer."
  (update-git-gutter-for-buffer (current-buffer))
  (message "Git gutter refreshed"))

(define-command git-gutter-toggle-line-highlight () ()
  "Toggle line highlighting for git gutter."
  (setf *git-gutter-highlight-line* (not *git-gutter-highlight-line*))
  (update-all-buffers)
  (message "Git gutter line highlight: ~A" (if *git-gutter-highlight-line* "on" "off")))

;;; Directory Mode Git Status Support

(defvar *directory-git-status-cache* (make-hash-table :test #'equal)
  "Cache for directory git status. Maps directory path to (timestamp . status-hash).")

(define-attribute git-status-modified-attribute
  (t :foreground "yellow"))

(define-attribute git-status-added-attribute
  (t :foreground "green"))

(define-attribute git-status-deleted-attribute
  (t :foreground "red"))

(define-attribute git-status-untracked-attribute
  (t :foreground "cyan"))

(define-attribute git-status-staged-attribute
  (t :foreground "green" :bold t))

(defun parse-git-status-porcelain (output)
  "Parse git status --porcelain=v1 output into a hash-table.
   Returns hash-table mapping relative-path -> status-keyword."
  (let ((status (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string output :separator '(#\Newline)))
      (when (>= (length line) 3)
        (let ((x (char line 0))
              (y (char line 1))
              (file (string-trim " " (subseq line 3))))
          (when (plusp (length file))
            ;; Handle renamed files: "R  old -> new"
            (when (find #\> file)
              (setf file (string-trim " " (subseq file (1+ (position #\> file))))))
            (cond
              ;; Untracked
              ((and (char= x #\?) (char= y #\?))
               (setf (gethash file status) :untracked))
              ;; Staged changes (index)
              ((char= x #\M) (setf (gethash file status) :staged-modified))
              ((char= x #\A) (setf (gethash file status) :staged-added))
              ((char= x #\D) (setf (gethash file status) :staged-deleted))
              ((char= x #\R) (setf (gethash file status) :staged-added))
              ;; Unstaged changes (work tree)
              ((char= y #\M) (setf (gethash file status) :modified))
              ((char= y #\D) (setf (gethash file status) :deleted)))))))
    status))

(defun parse-git-diff-name-status (output)
  "Parse git diff --name-status output into a hash-table.
   Returns hash-table mapping relative-path -> status-keyword."
  (let ((status (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string output :separator '(#\Newline)))
      (when (>= (length line) 2)
        (let* ((status-char (char line 0))
               (rest (string-trim '(#\Space #\Tab) (subseq line 1)))
               (file rest))
          ;; Handle renamed files: "R100\told\tnew" format
          ;; After stripping status char, rest is "100\told\tnew"
          (when (char= status-char #\R)
            (let* ((parts (uiop:split-string rest :separator '(#\Tab)))
                   (new-name (third parts)))
              (when new-name
                (setf file new-name))))
          (when (plusp (length file))
            (case status-char
              (#\M (setf (gethash file status) :modified))
              (#\A (setf (gethash file status) :added))
              (#\D (setf (gethash file status) :deleted))
              (#\R (setf (gethash file status) :added)))))))
    status))

(defun get-directory-git-status (directory)
  "Get git status for all files in directory. Uses cache when available.
   Compares against *git-gutter-ref* (default: HEAD)."
  (let* ((cache-key (format nil "~A:~A" (namestring directory) *git-gutter-ref*))
         (cached (gethash cache-key *directory-git-status-cache*))
         (now (get-internal-real-time))
         (ttl-ticks (* *directory-status-cache-ttl*
                       (/ internal-time-units-per-second 1000))))
    (if (and cached
             (< (- now (car cached)) ttl-ticks))
        (cdr cached)
        ;; Refresh cache
        (alexandria:when-let ((git-root (find-git-root directory)))
          (uiop:with-current-directory (git-root)
            (let* ((output (run-git (list "diff" "--name-status" *git-gutter-ref*)))
                   (status-table (parse-git-diff-name-status output)))
              (setf (gethash cache-key *directory-git-status-cache*)
                    (cons now status-table))
              status-table))))))

(defun find-status-in-directory (dir-prefix status-table)
  "Find if any file under dir-prefix has a git status.
   Returns the most significant status found (:modified > :added > :deleted > :untracked)."
  (let ((found-status nil))
    (maphash (lambda (path status)
               (when (and (> (length path) (length dir-prefix))
                          (string= dir-prefix (subseq path 0 (length dir-prefix))))
                 ;; Found a file under this directory
                 (case status
                   ((:modified :staged-modified)
                    (setf found-status :modified))
                   ((:added :staged-added)
                    (unless (eq found-status :modified)
                      (setf found-status :added)))
                   ((:deleted :staged-deleted)
                    (unless (member found-status '(:modified :added))
                      (setf found-status :deleted)))
                   (:untracked
                    (unless found-status
                      (setf found-status :untracked))))))
             status-table)
    found-status))

(defun get-file-git-status (pathname directory)
  "Get git status symbol for a specific file or directory.
   For directories, checks if any file underneath has changes."
  (alexandria:when-let ((status-table (get-directory-git-status directory)))
    (alexandria:when-let ((git-root (find-git-root directory)))
      (let ((relative-path (namestring (enough-namestring pathname git-root))))
        (if (uiop:directory-pathname-p pathname)
            ;; For directories, check if any files underneath have changes
            (let ((dir-prefix (if (alexandria:ends-with #\/ relative-path)
                                  relative-path
                                  (concatenate 'string relative-path "/"))))
              (find-status-in-directory dir-prefix status-table))
            ;; For files, direct lookup
            (gethash relative-path status-table))))))

(defun status-to-display (status)
  "Convert status keyword to display character and attribute."
  (case status
    (:modified (values "M" 'git-status-modified-attribute))
    (:staged-modified (values "M" 'git-status-staged-attribute))
    (:added (values "A" 'git-status-added-attribute))
    (:staged-added (values "A" 'git-status-staged-attribute))
    (:deleted (values "D" 'git-status-deleted-attribute))
    (:staged-deleted (values "D" 'git-status-staged-attribute))
    (:untracked (values "?" 'git-status-untracked-attribute))
    (otherwise (values " " nil))))

(defun insert-git-status (point item)
  "Inserter function for directory-mode to show git status.
   Skips the '..' (parent directory) entry."
  (let* ((pathname (item-pathname item))
         (directory (item-directory item)))
    ;; Skip ".." entry (when pathname is parent of directory)
    (if (and pathname directory
             (equal pathname (uiop:pathname-parent-directory-pathname directory)))
        (insert-string point "  ")
        (let ((status (when (and pathname directory)
                        (get-file-git-status pathname directory))))
          (multiple-value-bind (char attr)
              (status-to-display status)
            (if attr
                (insert-string point (format nil "~A " char) :attribute attr)
                (insert-string point "  ")))))))

(defun clear-directory-status-cache ()
  "Clear the directory git status cache."
  (clrhash *directory-git-status-cache*))

(define-command git-gutter-refresh-directory-status () ()
  "Refresh git status cache for current directory buffer."
  (let ((directory (buffer-directory (current-buffer))))
    (when directory
      (remhash (namestring directory) *directory-git-status-cache*)
      ;; Force redisplay of directory buffer if in directory-mode
      (when (eq (buffer-major-mode (current-buffer)) 'lem/directory-mode:directory-mode)
        (update-buffer (current-buffer)))
      (message "Git status refreshed"))))
