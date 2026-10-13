(defpackage :lem-tests/legit
  (:use :cl :lem :rove)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/legit)

;; Note: This test file uses internal symbols (::) from lem/legit package
;; because we need to test internal state management that is not part of
;; the public API. This is standard practice for unit testing internals.

;;; Helper functions and macros

(defun call-with-legit-variables-unbound (function)
  "Call FUNCTION with legit's special variables unbound."
  (when (boundp 'lem/legit::*peek-window*)
    (makunbound 'lem/legit::*peek-window*))
  (when (boundp 'lem/legit::*source-window*)
    (makunbound 'lem/legit::*source-window*))
  (when (boundp 'lem/legit::*parent-window*)
    (makunbound 'lem/legit::*parent-window*))
  (funcall function))

(defmacro with-legit-variables-unbound (&body body)
  "Execute BODY with legit's special variables unbound."
  `(call-with-legit-variables-unbound (lambda () ,@body)))

(defun cleanup-legit-windows ()
  "Clean up legit windows safely, switching away from them before deletion."
  (when (and (boundp 'lem/legit::*parent-window*)
             (not (lem:deleted-window-p lem/legit::*parent-window*)))
    (setf (lem:current-window) lem/legit::*parent-window*))
  (let ((lem/legit::*is-finalzing* t))
    (when (and (boundp 'lem/legit::*peek-window*)
               (not (lem:deleted-window-p lem/legit::*peek-window*)))
      (lem:delete-window lem/legit::*peek-window*))
    (when (and (boundp 'lem/legit::*source-window*)
               (not (lem:deleted-window-p lem/legit::*source-window*)))
      (lem:delete-window lem/legit::*source-window*))))

(defun call-with-temp-git-repo (function)
  "Call FUNCTION in a temporary git repository."
  (let ((temp-dir (uiop:ensure-directory-pathname
                   (format nil "~A/lem-test-~A/"
                           (uiop:temporary-directory)
                           (get-universal-time)))))
    (unwind-protect
         (progn
           (ensure-directories-exist temp-dir)
           (uiop:with-current-directory (temp-dir)
             ;; Initialize git repo
             (uiop:run-program '("git" "init") :ignore-error-status t)
             (uiop:run-program '("git" "config" "user.email" "test@test.com") :ignore-error-status t)
             (uiop:run-program '("git" "config" "user.name" "Test") :ignore-error-status t)
             ;; Create initial commit
             (with-open-file (s (merge-pathnames "README.md" temp-dir)
                                :direction :output :if-exists :supersede)
               (write-string "# Test" s))
             (uiop:run-program '("git" "add" ".") :ignore-error-status t)
             (uiop:run-program '("git" "commit" "-m" "Initial commit") :ignore-error-status t)
             (funcall function)))
      ;; Cleanup
      (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore))))

(defmacro with-temp-git-repo (&body body)
  "Execute BODY in a temporary git repository."
  `(call-with-temp-git-repo (lambda () ,@body)))

;;; Tests for legit-status-active-p

(deftest legit-status-active-p/unbound
  (testing "returns nil when *peek-window* is unbound"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          (ok (not (lem/legit::legit-status-active-p))))))))

(deftest legit-status-active-p/deleted-window
  (testing "returns nil when *peek-window* is deleted"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          ;; Create a floating window and then delete it
          (let* ((buffer (lem:make-buffer "*test-peek*" :temporary t))
                 (window (make-instance 'lem:floating-window
                                        :buffer buffer
                                        :x 0 :y 0
                                        :width 40 :height 20
                                        :use-modeline-p nil)))
            (setf lem/legit::*peek-window* window)
            ;; Delete the window
            (lem:delete-window window)
            (ok (not (lem/legit::legit-status-active-p)))))))))

(deftest legit-status-active-p/valid-window
  (testing "returns t when *peek-window* is valid"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          (let* ((buffer (lem:make-buffer "*test-peek*" :temporary t))
                 (window (make-instance 'lem:floating-window
                                        :buffer buffer
                                        :x 0 :y 0
                                        :width 40 :height 20
                                        :use-modeline-p nil)))
            (setf lem/legit::*peek-window* window)
            (ok (lem/legit::legit-status-active-p))
            ;; Cleanup
            (lem:delete-window window)))))))

;;; Tests for display function state management

(deftest display/initial-open-sets-parent-window
  (testing "initial display sets *parent-window* to current window"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          (let ((original-window (lem:current-window))
                (collector (make-instance 'lem/legit::collector
                                          :buffer (lem:make-buffer "*test-legit*" :temporary t))))
            (unwind-protect
                 (progn
                   ;; Call display
                   (lem/legit::display collector)
                   ;; Check that *parent-window* is set to original window
                   (ok (eq original-window lem/legit::*parent-window*))
                   ;; Check that new windows were created
                   (ok (boundp 'lem/legit::*peek-window*))
                   (ok (boundp 'lem/legit::*source-window*))
                   (ok (not (lem:deleted-window-p lem/legit::*peek-window*)))
                   (ok (not (lem:deleted-window-p lem/legit::*source-window*))))
              ;; Cleanup - always run
              (cleanup-legit-windows))))))))

(deftest display/refresh-preserves-parent-window
  (testing "refresh preserves *parent-window*"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          (let ((original-window (lem:current-window))
                (collector (make-instance 'lem/legit::collector
                                          :buffer (lem:make-buffer "*test-legit*" :temporary t))))
            (unwind-protect
                 (progn
                   ;; Initial display
                   (lem/legit::display collector)
                   (ok (eq original-window lem/legit::*parent-window*))
                   (let ((first-peek-window lem/legit::*peek-window*)
                         (first-source-window lem/legit::*source-window*))
                     ;; Refresh (call display again)
                     (let ((collector2 (make-instance 'lem/legit::collector
                                                      :buffer (lem:make-buffer "*test-legit-2*" :temporary t))))
                       (lem/legit::display collector2)
                       ;; *parent-window* should still be original window
                       (ok (eq original-window lem/legit::*parent-window*))
                       ;; Old windows should be deleted
                       (ok (lem:deleted-window-p first-peek-window))
                       (ok (lem:deleted-window-p first-source-window))
                       ;; New windows should exist
                       (ok (not (lem:deleted-window-p lem/legit::*peek-window*)))
                       (ok (not (lem:deleted-window-p lem/legit::*source-window*))))))
              ;; Cleanup - always run
              (cleanup-legit-windows))))))))

;;; Integration tests with temporary git repository

(deftest legit-status/open-and-close
  (testing "legit-status opens window and cleanup closes it"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          (with-temp-git-repo
            (unwind-protect
                 (progn
                   ;; Initially closed
                   (ok (not (lem/legit::legit-status-active-p)))
                   ;; Open with legit-status
                   (lem/legit:legit-status)
                   (ok (lem/legit::legit-status-active-p))
                   ;; Close by cleaning up windows directly
                   ;; (legit-quit uses async timer which doesn't work in test env)
                   (cleanup-legit-windows)
                   (ok (not (lem/legit::legit-status-active-p))))
              ;; Cleanup - always run
              (cleanup-legit-windows))))))))

(deftest legit-refresh/keeps-window-open
  (testing "legit-refresh keeps the legit window open"
    (with-current-buffers ()
      (with-fake-interface ()
        (with-legit-variables-unbound
          (with-temp-git-repo
            (unwind-protect
                 (progn
                   ;; Open legit
                   (lem/legit:legit-status)
                   (ok (lem/legit::legit-status-active-p))
                   (let ((original-parent lem/legit::*parent-window*))
                     ;; Refresh
                     (lem/legit:legit-refresh)
                     ;; Should still be open
                     (ok (lem/legit::legit-status-active-p))
                     ;; Parent window should be preserved
                     (ok (eq original-parent lem/legit::*parent-window*))))
              ;; Cleanup - always run
              (cleanup-legit-windows))))))))

;;; Tests for parse-github-url (lem/legit/utils package)

(deftest parse-github-url/ssh-with-git-suffix
  (testing "parses SSH URL with .git suffix"
    (multiple-value-bind (owner repo)
        (lem/legit/utils:parse-github-url "git@github.com:lem-project/lem.git")
      (ok (equal owner "lem-project"))
      (ok (equal repo "lem")))))

(deftest parse-github-url/ssh-without-git-suffix
  (testing "parses SSH URL without .git suffix"
    (multiple-value-bind (owner repo)
        (lem/legit/utils:parse-github-url "git@github.com:lem-project/lem")
      (ok (equal owner "lem-project"))
      (ok (equal repo "lem")))))

(deftest parse-github-url/https-with-git-suffix
  (testing "parses HTTPS URL with .git suffix"
    (multiple-value-bind (owner repo)
        (lem/legit/utils:parse-github-url "https://github.com/lem-project/lem.git")
      (ok (equal owner "lem-project"))
      (ok (equal repo "lem")))))

(deftest parse-github-url/https-without-git-suffix
  (testing "parses HTTPS URL without .git suffix"
    (multiple-value-bind (owner repo)
        (lem/legit/utils:parse-github-url "https://github.com/lem-project/lem")
      (ok (equal owner "lem-project"))
      (ok (equal repo "lem")))))

(deftest parse-github-url/http-url
  (testing "parses HTTP URL"
    (multiple-value-bind (owner repo)
        (lem/legit/utils:parse-github-url "http://github.com/owner/repo")
      (ok (equal owner "owner"))
      (ok (equal repo "repo")))))

(deftest parse-github-url/with-whitespace
  (testing "trims whitespace from URL"
    (multiple-value-bind (owner repo)
        (lem/legit/utils:parse-github-url "  git@github.com:owner/repo.git  ")
      (ok (equal owner "owner"))
      (ok (equal repo "repo")))))

(deftest parse-github-url/non-github-url
  (testing "returns nil for non-GitHub URL"
    (ok (null (lem/legit/utils:parse-github-url "git@gitlab.com:owner/repo.git")))
    (ok (null (lem/legit/utils:parse-github-url "https://bitbucket.org/owner/repo")))))

(deftest parse-github-url/invalid-url
  (testing "returns nil for invalid URL"
    (ok (null (lem/legit/utils:parse-github-url "not-a-url")))
    (ok (null (lem/legit/utils:parse-github-url "")))))

;;; Tests for build-github-url (lem/legit/utils package)

(deftest build-github-url/basic
  (testing "builds basic URL without line number"
    (ok (equal (lem/legit/utils:build-github-url "owner" "repo" "main" "src/file.lisp")
               "https://github.com/owner/repo/blob/main/src/file.lisp"))))

(deftest build-github-url/with-line-number
  (testing "builds URL with single line number"
    (ok (equal (lem/legit/utils:build-github-url "owner" "repo" "main" "src/file.lisp"
                                                  :start-line 42)
               "https://github.com/owner/repo/blob/main/src/file.lisp#L42"))))

(deftest build-github-url/with-line-range
  (testing "builds URL with line range"
    (ok (equal (lem/legit/utils:build-github-url "owner" "repo" "main" "src/file.lisp"
                                                  :start-line 10 :end-line 20)
               "https://github.com/owner/repo/blob/main/src/file.lisp#L10-L20"))))

(deftest build-github-url/same-start-end-line
  (testing "builds URL with single line when start equals end"
    (ok (equal (lem/legit/utils:build-github-url "owner" "repo" "feature/test" "README.md"
                                                  :start-line 5 :end-line 5)
               "https://github.com/owner/repo/blob/feature/test/README.md#L5"))))

(deftest build-github-url/with-branch-containing-slash
  (testing "builds URL with branch containing slash"
    (ok (equal (lem/legit/utils:build-github-url "owner" "repo" "feature/new-feature" "file.lisp"
                                                  :start-line 1)
               "https://github.com/owner/repo/blob/feature/new-feature/file.lisp#L1"))))
