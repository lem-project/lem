(uiop:define-package :lem/legit/browse
  (:use :cl :lem)
  (:import-from :lem/legit
                :with-current-project)
  (:import-from :lem/legit/utils
                :parse-github-url
                :build-github-url)
  (:export :legit-browse-remote
           :*default-remote*
           ;; Re-export from lem/legit/utils for backward compatibility
           :parse-github-url
           :build-github-url)
  (:documentation "Open current file location in GitHub.

This module provides functionality to open the current file at the current line
(or region) in a web browser on GitHub."))

(in-package :lem/legit/browse)

(defvar *default-remote* "origin"
  "Default remote name to use for browsing.")

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
