
(in-package :lem/legit)

#|
Open current file location in GitHub.

This module provides functionality to open the current file at the current line
(or region) in a web browser on GitHub.
|#

(defvar *default-remote* "origin"
  "Default remote name to use for browsing.")

(defun parse-github-url (url)
  "Parse a Git remote URL and return (values owner repo) for GitHub.
   Supports:
   - git@github.com:owner/repo.git
   - git@github.com:owner/repo
   - https://github.com/owner/repo.git
   - https://github.com/owner/repo
   Returns NIL if the URL is not a GitHub URL."
  (let ((url (str:trim url)))
    ;; SSH format: git@github.com:owner/repo or git@github.com:owner/repo.git
    (ppcre:register-groups-bind (owner repo)
        ("^git@github\\.com:([^/]+)/(.+?)(?:\\.git)?$" url)
      (return-from parse-github-url (values owner repo)))
    ;; HTTPS format: https://github.com/owner/repo or https://github.com/owner/repo.git
    (ppcre:register-groups-bind (owner repo)
        ("^https?://github\\.com/([^/]+)/(.+?)(?:\\.git)?$" url)
      (return-from parse-github-url (values owner repo)))
    ;; No match
    nil))

(defun build-github-url (owner repo branch path &key start-line end-line)
  "Build a GitHub blob URL with optional line number fragment.
   URL format: https://github.com/{owner}/{repo}/blob/{branch}/{path}#L{line}
   For line ranges: #L{start}-L{end}"
  (let ((line-fragment
          (cond
            ((and start-line end-line (not (= start-line end-line)))
             (format nil "#L~A-L~A" start-line end-line))
            (start-line
             (format nil "#L~A" start-line))
            (t ""))))
    (format nil "https://github.com/~A/~A/blob/~A/~A~A"
            owner repo branch path line-fragment)))

(defun get-line-range ()
  "Get the current line or line range if region is active.
   Returns (values start-line end-line).
   If no region is active, end-line will be NIL."
  (let ((buffer (current-buffer)))
    (if (buffer-mark-p buffer)
        (let ((start (region-beginning buffer))
              (end (region-end buffer)))
          (values (line-number-at-point start)
                  (line-number-at-point end)))
        (values (line-number-at-point (current-point))
                nil))))

(defun get-relative-path (filepath git-root)
  "Get the relative path from git root to the file."
  (enough-namestring (pathname filepath) (pathname git-root)))

(defun browse-file-at-github ()
  "Open the current file at the current line/region in GitHub."
  (let ((filepath (buffer-filename (current-buffer))))
    (unless filepath
      (editor-error "Buffer is not visiting a file"))

    (with-current-project (vcs)
      (let* ((remote-url (lem/porcelain:remote-url vcs :remote *default-remote*))
             (branch (lem/porcelain:current-branch vcs))
             (git-root (uiop:getcwd)))

        (unless remote-url
          (editor-error "No remote URL found for '~A'" *default-remote*))

        (multiple-value-bind (owner repo)
            (parse-github-url remote-url)
          (unless owner
            (editor-error "Not a GitHub repository: ~A" remote-url))

          (let ((relative-path (get-relative-path filepath git-root)))
            (multiple-value-bind (start-line end-line)
                (get-line-range)
              (let ((url (build-github-url owner repo branch relative-path
                                           :start-line start-line
                                           :end-line end-line)))
                (open-external-file url)
                (message "Opened: ~A" url)))))))))

(define-command legit-browse-remote () ()
  "Open the current file at the current line in GitHub.
   If a region is active, the line range will be selected in GitHub."
  (browse-file-at-github))

(define-key *global-keymap* "C-c g b" 'legit-browse-remote)
