(defpackage :lem-git-gutter/tests
  (:use :cl :rove)
  (:local-nicknames (:parser :lem-git-gutter/diff-parser)))
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
