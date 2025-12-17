(defpackage :lem-tests/lisp-mode/file-conversion
  (:use :cl :rove)
  (:import-from :lem-lisp-mode))
(in-package :lem-tests/lisp-mode/file-conversion)

;;; Tests for glob pattern functions

(deftest has-glob-wildcard-p
  (testing "detects single wildcard"
    (ok (lem-lisp-mode/internal::has-glob-wildcard-p "/nix/store/*/"))
    (ok (lem-lisp-mode/internal::has-glob-wildcard-p "*-alexandria-*")))
  (testing "detects double wildcard"
    (ok (lem-lisp-mode/internal::has-glob-wildcard-p "**/*.lisp"))
    (ok (lem-lisp-mode/internal::has-glob-wildcard-p "/foo/**/bar")))
  (testing "returns nil for no wildcards"
    (ng (lem-lisp-mode/internal::has-glob-wildcard-p "/app/src/"))
    (ng (lem-lisp-mode/internal::has-glob-wildcard-p "simple-path"))))

(deftest glob-pattern-to-regex
  (testing "converts single wildcard to regex"
    (ok (equal "^/nix/store/[^/]*/"
               (lem-lisp-mode/internal::glob-pattern-to-regex "/nix/store/*/")))
    (ok (equal "^[^/]*-alexandria-[^/]*"
               (lem-lisp-mode/internal::glob-pattern-to-regex "*-alexandria-*"))))
  (testing "converts double wildcard to regex"
    (ok (equal "^.*/[^/]*\\.lisp"
               (lem-lisp-mode/internal::glob-pattern-to-regex "**/*.lisp"))))
  (testing "escapes regex special characters"
    (ok (equal "^foo\\.bar"
               (lem-lisp-mode/internal::glob-pattern-to-regex "foo.bar")))
    (ok (equal "^\\(test\\)"
               (lem-lisp-mode/internal::glob-pattern-to-regex "(test)")))))

(deftest match-glob-pattern
  (testing "matches single wildcard pattern"
    (multiple-value-bind (matched prefix rest)
        (lem-lisp-mode/internal::match-glob-pattern
         "/nix/store/*/"
         "/nix/store/abc123-foo/")
      (ok matched)
      (ok (equal "/nix/store/abc123-foo/" prefix))
      (ok (equal "" rest))))
  (testing "captures rest of path after match"
    (multiple-value-bind (matched prefix rest)
        (lem-lisp-mode/internal::match-glob-pattern
         "/nix/store/*-alexandria-*/"
         "/nix/store/abc-sbcl-alexandria-20241012-git/src/foo.lisp")
      (ok matched)
      (ok (equal "/nix/store/abc-sbcl-alexandria-20241012-git/" prefix))
      (ok (equal "src/foo.lisp" rest))))
  (testing "returns nil for non-matching pattern"
    (multiple-value-bind (matched prefix rest)
        (lem-lisp-mode/internal::match-glob-pattern
         "/nix/store/*-alexandria-*/"
         "/home/user/project/src/foo.lisp")
      (declare (ignore prefix rest))
      (ng matched)))
  (testing "works with simple prefix (no wildcard)"
    (multiple-value-bind (matched prefix rest)
        (lem-lisp-mode/internal::match-glob-pattern
         "/app/"
         "/app/src/main.lisp")
      (ok matched)
      (ok (equal "/app/" prefix))
      (ok (equal "src/main.lisp" rest)))))

(deftest expand-path
  (testing "expands home directory"
    (ok (equal (namestring (user-homedir-pathname))
               (lem-lisp-mode/internal::expand-path "~")))
    (ok (alexandria:starts-with-subseq
         (namestring (user-homedir-pathname))
         (lem-lisp-mode/internal::expand-path "~/foo"))))
  (testing "keeps absolute paths as-is"
    (ok (equal "/absolute/path"
               (lem-lisp-mode/internal::expand-path "/absolute/path"))))
  (testing "expands relative paths from current directory"
    (let ((cwd (uiop:getcwd)))
      (ok (alexandria:starts-with-subseq
           (namestring cwd)
           (lem-lisp-mode/internal::expand-path "./"))))))

(deftest convert-remote-to-local-file-simple
  (testing "backward compatibility: simple prefix match"
    (let ((lem-lisp-mode/internal::*file-conversion-map*
            '(("/app/" . "/home/user/project/"))))
      (ok (equal "/home/user/project/src/main.lisp"
                 (lem-lisp-mode/internal::convert-remote-to-local-file
                  "/app/src/main.lisp")))))
  (testing "returns original if no match"
    (let ((lem-lisp-mode/internal::*file-conversion-map*
            '(("/app/" . "/home/user/project/"))))
      (ok (equal "/other/path/file.lisp"
                 (lem-lisp-mode/internal::convert-remote-to-local-file
                  "/other/path/file.lisp"))))))

(deftest convert-remote-to-local-file-glob
  (testing "glob pattern with existing directory"
    ;; Create a temp directory structure for testing
    (let* ((temp-dir (uiop:temporary-directory))
           (test-dir (merge-pathnames "lem-test-glob/" temp-dir))
           (lem-lisp-mode/internal::*file-conversion-map*
             `(("/nix/store/*-test-lib/" . ,(namestring test-dir)))))
      (ensure-directories-exist test-dir)
      (unwind-protect
           (progn
             (ok (alexandria:starts-with-subseq
                  (namestring test-dir)
                  (lem-lisp-mode/internal::convert-remote-to-local-file
                   "/nix/store/abc123-test-lib/src/foo.lisp"))))
        (uiop:delete-directory-tree test-dir :validate t :if-does-not-exist :ignore))))
  (testing "glob pattern returns original if local dir not found"
    (let ((lem-lisp-mode/internal::*file-conversion-map*
            '(("/nix/store/*-nonexistent/" . "/nonexistent/path/"))))
      (ok (equal "/nix/store/abc-nonexistent/src/foo.lisp"
                 (lem-lisp-mode/internal::convert-remote-to-local-file
                  "/nix/store/abc-nonexistent/src/foo.lisp"))))))

(deftest convert-local-to-remote-file
  (testing "simple reverse conversion"
    (let ((lem-lisp-mode/internal::*file-conversion-map*
            '(("/app/" . "/home/user/project/"))))
      (ok (equal "/app/src/main.lisp"
                 (lem-lisp-mode/internal::convert-local-to-remote-file
                  "/home/user/project/src/main.lisp")))))
  (testing "skips glob patterns in local pattern"
    (let ((lem-lisp-mode/internal::*file-conversion-map*
            '(("/nix/store/*-lib/" . "/local/*-lib/"))))
      ;; Should return original since local pattern has wildcard
      (ok (equal "/local/foo-lib/src/main.lisp"
                 (lem-lisp-mode/internal::convert-local-to-remote-file
                  "/local/foo-lib/src/main.lisp"))))))
