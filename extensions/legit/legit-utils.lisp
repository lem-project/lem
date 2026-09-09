(uiop:define-package :lem/legit/utils
  (:use :cl)
  (:export :parse-github-url
           :build-github-url)
  (:documentation "Utility functions for legit VCS integration.

This package provides reusable URL parsing and building utilities
for GitHub and potentially other Git hosting services."))

(in-package :lem/legit/utils)

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
