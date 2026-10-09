(defpackage :lem-tests/language-mode
  (:use :cl :rove))
(in-package :lem-tests/language-mode)

(defun make-test-directory ()
  (loop
    :for directory := (merge-pathnames
                       (format nil ".lem-language-mode-test-~A/"
                               (gensym "ROOT-"))
                       (user-homedir-pathname))
    :unless (probe-file directory)
      :do (ensure-directories-exist directory)
          (return directory)))

(defmacro with-test-directory ((directory) &body body)
  `(let ((,directory (make-test-directory)))
     (unwind-protect
          (progn ,@body)
       (uiop:delete-directory-tree
        ,directory
        :validate t
        :if-does-not-exist :ignore))))

(defun ensure-test-directory (pathname)
  (ensure-directories-exist (uiop:ensure-directory-pathname pathname)))

(defun touch-file (pathname)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-line "" stream))
  pathname)

(defun same-path-p (expected actual)
  (uiop:pathname-equal expected actual))

(defun signals-error-p (function)
  (handler-case
      (progn
        (funcall function)
        nil)
    (error () t)))

(deftest find-root-directory/string-pattern-matches-directory
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      ;; The file marker at BASE keeps the old implementation finite.
      (touch-file (uiop:subpathname base ".git"))
      (ensure-test-directory (uiop:subpathname project ".git/"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory src '(".git"))))
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory src '(".git/")))))))

(deftest find-root-directory/string-pattern-matches-file
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (touch-file (uiop:subpathname project ".git"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory src '(".git")))))))

(deftest find-root-directory/string-pattern-does-not-use-substring-match
  (with-test-directory (base)
    (testing ".git does not match .gitconfig"
      (let* ((child (uiop:subpathname base "child/"))
             (src (uiop:subpathname child "src/")))
        (touch-file (uiop:subpathname base ".git"))
        (touch-file (uiop:subpathname child ".gitconfig"))
        (ensure-test-directory src)
        (ok (same-path-p
             base
             (lem/language-mode:find-root-directory src '(".git"))))))
    (testing "package.json does not match package.json.bak"
      (let* ((child (uiop:subpathname base "package-child/"))
             (src (uiop:subpathname child "src/")))
        (touch-file (uiop:subpathname base "package.json"))
        (touch-file (uiop:subpathname child "package.json.bak"))
        (ensure-test-directory src)
        (ok (same-path-p
             base
             (lem/language-mode:find-root-directory
              src
              '("package.json"))))))))

(deftest find-root-directory/string-pattern-matches-nested-relative-path
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (touch-file (uiop:subpathname base "root-sentinel"))
      (touch-file (uiop:subpathname project ".lsp/config.edn"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory
            src
            (list ".lsp/config.edn"
                  (lambda (name)
                    (string= name "root-sentinel")))))))))

(deftest find-root-directory/default-pattern-matches-git-directory
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      ;; Old default callback can detect this file but not PROJECT/.git/.
      (touch-file (uiop:subpathname base ".git"))
      (ensure-test-directory (uiop:subpathname project ".git/"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory src nil))))))

(deftest find-root-directory/nearest-ancestor-wins
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (touch-file (uiop:subpathname base "package.json"))
      (touch-file (uiop:subpathname project "package.json"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory
            src
            '("package.json")))))))

(deftest find-root-directory/multiple-markers-on-same-candidate
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (ensure-test-directory (uiop:subpathname project ".git/"))
      (touch-file (uiop:subpathname project "package.json"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory
            src
            '(".git" "package.json")))))))

(deftest find-root-directory/preserves-fallback
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (ensure-test-directory src)
      (ok (same-path-p
           src
           (lem/language-mode:find-root-directory
            src
            (list (lambda (name)
                    (declare (ignore name))
                    nil))))))))

(deftest find-root-directory/accepts-directory-string
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (touch-file (uiop:subpathname project "package.json"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory
            (namestring src)
            '("package.json")))))))

(deftest find-root-directory/function-pattern-keeps-file-namestring-contract
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (subdir (uiop:subpathname project "directory/"))
           (sentinel (uiop:subpathname project "sentinel"))
           (names '()))
      (ensure-test-directory subdir)
      (touch-file sentinel)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory
            project
            (list (lambda (name)
                    ;; Inspect all direct entries regardless of listing order.
                    (push name names)
                    nil)
                  "sentinel"))))
      (ok (member "" names :test #'string=)
          "directory entries should still be passed as an empty file-namestring")
      (ok (member "sentinel" names :test #'string=)
          "file entries should still be passed by file-namestring"))))

(deftest find-root-directory/mixed-string-and-function-patterns
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/")))
      (touch-file (uiop:subpathname project "sentinel"))
      (ensure-test-directory src)
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory
            src
            (list "missing.marker"
                  (lambda (name)
                    (string= name "sentinel")))))))))

(deftest find-root-directory/rejects-invalid-string-patterns
  (with-test-directory (base)
    (dolist (pattern '("" "." "../sentinel" "/absolute" "*.asd" "file?.json"))
      (ok (signals-error-p
           (lambda ()
             (lem/language-mode:find-root-directory
              base
              (list pattern))))
          (format nil "invalid root pattern should signal an error: ~S"
                  pattern)))))

(deftest lisp-asd-root-pattern-matches-exact-extension
  (with-test-directory (base)
    (let* ((project (uiop:subpathname base "project/"))
           (src (uiop:subpathname project "src/"))
           (patterns (list #'lem-lisp-mode:asdf-root-file-p)))
      (touch-file (uiop:subpathname base "parent.asd"))
      (touch-file (uiop:subpathname project "sample.asd.BACK"))
      (ensure-test-directory src)
      (ok (same-path-p
           base
           (lem/language-mode:find-root-directory src patterns))
          "backup file must not be treated as an ASDF system")
      (touch-file (uiop:subpathname project "sample.asd"))
      (ok (same-path-p
           project
           (lem/language-mode:find-root-directory src patterns))
          ".asd file must identify the nearest project root")
      (ok (lem-lisp-mode:asdf-root-file-p "sample.asd"))
      (ok (not (lem-lisp-mode:asdf-root-file-p "sample.asd.BACK")))
      (ok (not (lem-lisp-mode:asdf-root-file-p "sample.asd.bak")))
      (ok (not (lem-lisp-mode:asdf-root-file-p "README"))))))
