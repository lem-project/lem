(defpackage :lem-git-gutter/tests
  (:use :cl :rove)
  (:local-nicknames (:parser :lem-git-gutter/diff-parser)
                    (:gutter :lem-git-gutter)))
(in-package :lem-git-gutter/tests)

;;; Test parse-hunk-header

(deftest test-parse-hunk-header-basic
  (multiple-value-bind (start count)
      (parser:parse-hunk-header "@@ -10,3 +15,5 @@")
    (ok (= start 15) "new-start should be 15")
    (ok (= count 5) "new-count should be 5")))

(deftest test-parse-hunk-header-single-line
  (multiple-value-bind (start count)
      (parser:parse-hunk-header "@@ -1 +1 @@")
    (ok (= start 1) "new-start should be 1")
    (ok (= count 1) "new-count should default to 1")))

(deftest test-parse-hunk-header-with-context
  (multiple-value-bind (start count)
      (parser:parse-hunk-header "@@ -1,2 +1,4 @@ function-name")
    (ok (= start 1) "new-start should be 1")
    (ok (= count 4) "new-count should be 4")))

(deftest test-parse-hunk-header-zero-count
  (multiple-value-bind (start count)
      (parser:parse-hunk-header "@@ -5,2 +5,0 @@")
    (ok (= start 5) "new-start should be 5")
    (ok (= count 0) "new-count should be 0")))

;;; Test parse-git-diff

(deftest test-parse-git-diff-added-lines
  (let* ((diff-output "diff --git a/test.txt b/test.txt
index abc123..def456 100644
--- a/test.txt
+++ b/test.txt
@@ -1,0 +1,2 @@
+added line 1
+added line 2")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 1 changes) :added) "Line 1 should be added")
    (ok (eq (gethash 2 changes) :added) "Line 2 should be added")
    (ok (null (gethash 3 changes)) "Line 3 should have no change")))

(deftest test-parse-git-diff-deleted-lines
  (let* ((diff-output "diff --git a/test.txt b/test.txt
index abc123..def456 100644
--- a/test.txt
+++ b/test.txt
@@ -1,2 +1,0 @@
-deleted line 1
-deleted line 2")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 1 changes) :deleted) "Line 1 should show deletion marker")))

(deftest test-parse-git-diff-modified-lines
  (let* ((diff-output "diff --git a/test.txt b/test.txt
index abc123..def456 100644
--- a/test.txt
+++ b/test.txt
@@ -1,1 +1,1 @@
-old line
+new line")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 1 changes) :modified) "Line 1 should be modified")))

(deftest test-parse-git-diff-multiple-modifications
  (let* ((diff-output "diff --git a/test.txt b/test.txt
index abc123..def456 100644
--- a/test.txt
+++ b/test.txt
@@ -2,2 +2,2 @@
-old line 2
-old line 3
+new line 2
+new line 3")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 2 changes) :modified) "Line 2 should be modified")
    (ok (eq (gethash 3 changes) :modified) "Line 3 should be modified")))

(deftest test-parse-git-diff-mixed-changes
  (let* ((diff-output "diff --git a/test.txt b/test.txt
index abc123..def456 100644
--- a/test.txt
+++ b/test.txt
@@ -1,0 +1,1 @@
+new line at start
@@ -5,1 +6,1 @@
-old line 5
+modified line 6
@@ -10,2 +11,0 @@
-deleted 1
-deleted 2")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 1 changes) :added) "Line 1 should be added")
    (ok (eq (gethash 6 changes) :modified) "Line 6 should be modified")
    (ok (eq (gethash 11 changes) :deleted) "Line 11 should show deletion")))

(deftest test-parse-git-diff-empty-output
  (let ((changes (parser:parse-git-diff "")))
    (ok (= (hash-table-count changes) 0) "Empty diff should have no changes")))

(deftest test-parse-git-diff-multiple-hunks
  (let* ((diff-output "diff --git a/test.txt b/test.txt
index abc123..def456 100644
--- a/test.txt
+++ b/test.txt
@@ -1,0 +1,1 @@
+line 1
@@ -10,0 +11,1 @@
+line 11
@@ -20,0 +22,1 @@
+line 22")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 1 changes) :added) "Line 1 should be added")
    (ok (eq (gethash 11 changes) :added) "Line 11 should be added")
    (ok (eq (gethash 22 changes) :added) "Line 22 should be added")
    (ok (= (hash-table-count changes) 3) "Should have exactly 3 changes")))

(deftest test-parse-git-diff-delete-then-add-more
  (let* ((diff-output "diff --git a/test.txt b/test.txt
@@ -5,1 +5,3 @@
-old
+new1
+new2
+new3")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 5 changes) :modified) "Line 5 should be modified (delete+add)")
    (ok (eq (gethash 6 changes) :added) "Line 6 should be added")
    (ok (eq (gethash 7 changes) :added) "Line 7 should be added")))

;;; Test git diff --no-index output format (for unsaved buffers)

(deftest test-parse-git-diff-no-index-format
  "Test parsing git diff --no-index output (used for unsaved buffer comparison)"
  (let* ((diff-output "diff --git a/tmp/orig.txt b/tmp/new.txt
index a92d664b..7c7d6265 100644
--- a/tmp/orig.txt
+++ b/tmp/new.txt
@@ -2 +2 @@ line 1
-line 2
+modified line 2
@@ -3,0 +4 @@ line 3
+added line 4")
         (changes (parser:parse-git-diff diff-output)))
    (ok (eq (gethash 2 changes) :modified) "Line 2 should be modified")
    (ok (eq (gethash 4 changes) :added) "Line 4 should be added")
    (ok (= (hash-table-count changes) 2) "Should have exactly 2 changes")))

;;; Test parse-git-status-porcelain (directory status)

(deftest test-parse-git-status-porcelain-untracked
  "Test parsing untracked files"
  (let* ((output "?? newfile.txt
?? another/path.lisp")
         (status (gutter:parse-git-status-porcelain output)))
    (ok (eq (gethash "newfile.txt" status) :untracked)
        "newfile.txt should be untracked")
    (ok (eq (gethash "another/path.lisp" status) :untracked)
        "another/path.lisp should be untracked")))

(deftest test-parse-git-status-porcelain-modified
  "Test parsing modified files (unstaged)"
  (let* ((output " M src/file.lisp
 M tests/test.lisp")
         (status (gutter:parse-git-status-porcelain output)))
    (ok (eq (gethash "src/file.lisp" status) :modified)
        "src/file.lisp should be modified")
    (ok (eq (gethash "tests/test.lisp" status) :modified)
        "tests/test.lisp should be modified")))

(deftest test-parse-git-status-porcelain-staged
  "Test parsing staged files"
  (let* ((output "M  staged-mod.lisp
A  new-staged.lisp
D  deleted-staged.lisp")
         (status (gutter:parse-git-status-porcelain output)))
    (ok (eq (gethash "staged-mod.lisp" status) :staged-modified)
        "staged-mod.lisp should be staged-modified")
    (ok (eq (gethash "new-staged.lisp" status) :staged-added)
        "new-staged.lisp should be staged-added")
    (ok (eq (gethash "deleted-staged.lisp" status) :staged-deleted)
        "deleted-staged.lisp should be staged-deleted")))

(deftest test-parse-git-status-porcelain-deleted
  "Test parsing deleted files (unstaged)"
  (let* ((output " D removed.lisp")
         (status (gutter:parse-git-status-porcelain output)))
    (ok (eq (gethash "removed.lisp" status) :deleted)
        "removed.lisp should be deleted")))

(deftest test-parse-git-status-porcelain-renamed
  "Test parsing renamed files"
  (let* ((output "R  old-name.lisp -> new-name.lisp")
         (status (gutter:parse-git-status-porcelain output)))
    (ok (eq (gethash "new-name.lisp" status) :staged-added)
        "new-name.lisp (renamed) should be staged-added")))

(deftest test-parse-git-status-porcelain-mixed
  "Test parsing mixed status output"
  (let* ((output "?? untracked.txt
 M modified.lisp
M  staged.lisp
A  added.lisp
 D deleted.lisp")
         (status (gutter:parse-git-status-porcelain output)))
    (ok (= (hash-table-count status) 5) "Should have 5 entries")
    (ok (eq (gethash "untracked.txt" status) :untracked))
    (ok (eq (gethash "modified.lisp" status) :modified))
    (ok (eq (gethash "staged.lisp" status) :staged-modified))
    (ok (eq (gethash "added.lisp" status) :staged-added))
    (ok (eq (gethash "deleted.lisp" status) :deleted))))

(deftest test-parse-git-status-porcelain-empty
  "Test parsing empty output"
  (let ((status (gutter:parse-git-status-porcelain "")))
    (ok (= (hash-table-count status) 0) "Empty output should have no entries")))

;;; Test status-to-display

(deftest test-status-to-display-modified
  "Test display conversion for modified status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display :modified)
    (ok (string= char "M") "Modified should display as M")
    (ok (eq attr 'gutter::git-status-modified-attribute))))

(deftest test-status-to-display-staged-modified
  "Test display conversion for staged-modified status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display :staged-modified)
    (ok (string= char "M") "Staged-modified should display as M")
    (ok (eq attr 'gutter::git-status-staged-attribute))))

(deftest test-status-to-display-added
  "Test display conversion for added status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display :added)
    (ok (string= char "A") "Added should display as A")
    (ok (eq attr 'gutter::git-status-added-attribute))))

(deftest test-status-to-display-staged-added
  "Test display conversion for staged-added status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display :staged-added)
    (ok (string= char "A") "Staged-added should display as A")
    (ok (eq attr 'gutter::git-status-staged-attribute))))

(deftest test-status-to-display-deleted
  "Test display conversion for deleted status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display :deleted)
    (ok (string= char "D") "Deleted should display as D")
    (ok (eq attr 'gutter::git-status-deleted-attribute))))

(deftest test-status-to-display-untracked
  "Test display conversion for untracked status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display :untracked)
    (ok (string= char "?") "Untracked should display as ?")
    (ok (eq attr 'gutter::git-status-untracked-attribute))))

(deftest test-status-to-display-nil
  "Test display conversion for nil/no status"
  (multiple-value-bind (char attr)
      (gutter:status-to-display nil)
    (ok (string= char " ") "No status should display as space")
    (ok (null attr) "No status should have nil attribute")))

;;; Test find-status-in-directory (directory status aggregation)

(deftest test-find-status-in-directory-modified
  "Test finding modified status in subdirectory"
  (let* ((output " M src/foo/bar.lisp
 M src/foo/baz.lisp")
         (status (gutter:parse-git-status-porcelain output))
         (result (lem-git-gutter::find-status-in-directory "src/foo/" status)))
    (ok (eq result :modified) "Directory with modified files should show :modified")))

(deftest test-find-status-in-directory-mixed
  "Test priority when directory has mixed statuses"
  (let* ((output "?? src/sub/untracked.txt
A  src/sub/added.lisp
 M src/sub/modified.lisp")
         (status (gutter:parse-git-status-porcelain output))
         (result (lem-git-gutter::find-status-in-directory "src/sub/" status)))
    (ok (eq result :modified) "Modified should take priority over added and untracked")))

(deftest test-find-status-in-directory-added-only
  "Test directory with only added files"
  (let* ((output "A  lib/new/file1.lisp
A  lib/new/file2.lisp")
         (status (gutter:parse-git-status-porcelain output))
         (result (lem-git-gutter::find-status-in-directory "lib/new/" status)))
    (ok (eq result :added) "Directory with only added files should show :added")))

(deftest test-find-status-in-directory-untracked-only
  "Test directory with only untracked files"
  (let* ((output "?? docs/draft/readme.md")
         (status (gutter:parse-git-status-porcelain output))
         (result (lem-git-gutter::find-status-in-directory "docs/draft/" status)))
    (ok (eq result :untracked) "Directory with only untracked files should show :untracked")))

(deftest test-find-status-in-directory-no-match
  "Test directory with no matching files"
  (let* ((output " M other/file.lisp")
         (status (gutter:parse-git-status-porcelain output))
         (result (lem-git-gutter::find-status-in-directory "src/" status)))
    (ok (null result) "Directory with no matching files should return nil")))

(deftest test-find-status-in-directory-nested
  "Test nested directory detection"
  (let* ((output " M a/b/c/deep.lisp")
         (status (gutter:parse-git-status-porcelain output))
         (result-a (lem-git-gutter::find-status-in-directory "a/" status))
         (result-ab (lem-git-gutter::find-status-in-directory "a/b/" status))
         (result-abc (lem-git-gutter::find-status-in-directory "a/b/c/" status)))
    (ok (eq result-a :modified) "Parent dir 'a/' should show modified")
    (ok (eq result-ab :modified) "Parent dir 'a/b/' should show modified")
    (ok (eq result-abc :modified) "Parent dir 'a/b/c/' should show modified")))
