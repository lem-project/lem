(defpackage :lem-git-gutter
  (:use :cl :lem)
  (:local-nicknames (:diff-parser :lem-git-gutter/diff-parser))
  (:export :git-gutter-mode
           :git-gutter-set-ref
           :git-gutter-refresh
           :git-gutter-toggle-line-highlight
           :*git-gutter-ref*
           :*git-gutter-highlight-line*))
(in-package :lem-git-gutter)

;;; Configuration

(defvar *git-gutter-ref* "HEAD"
  "The git ref to compare against. Defaults to HEAD.")

(defvar *git-gutter-highlight-line* nil
  "When non-nil, highlight the entire line for added/modified lines.")

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
  "Get git diff for a buffer's file.
   Returns the diff output string or nil."
  (alexandria:when-let ((filepath (buffer-filename buffer)))
    (let ((directory (or (buffer-directory buffer)
                         (uiop:pathname-directory-pathname filepath))))
      (alexandria:when-let ((git-root (find-git-root directory)))
        (uiop:with-current-directory (git-root)
          (let ((relative-path (enough-namestring filepath git-root)))
            (run-git (list "diff" "--no-color" "-U0" ref "--" relative-path))))))))

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
  "Update the git gutter diff information for buffer."
  (let ((diff-output (get-git-diff-for-buffer buffer *git-gutter-ref*)))
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

;;; Mode Definition

(defun git-gutter-after-save (buffer)
  "Hook function called after saving a buffer."
  (when (buffer-filename buffer)
    (update-git-gutter-for-buffer buffer)))

(defun enable-hook ()
  "Called when git-gutter-mode is enabled."
  (update-all-buffers)
  (add-hook (variable-value 'after-save-hook :global t)
            'git-gutter-after-save))

(defun disable-hook ()
  "Called when git-gutter-mode is disabled."
  (remove-hook (variable-value 'after-save-hook :global t)
               'git-gutter-after-save)
  (clear-all-buffers))

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
