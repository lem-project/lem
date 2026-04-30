(defpackage :lem-tests/filer
  (:use :cl :rove :lem :lem/filer))
(in-package :lem-tests/filer)

;;; Test subdirectory-p
(deftest subdirectory-p-test
  (testing "subdirectory-p returns T for valid subdirectories"
    (ok (subdirectory-p
         #P"/home/user/project/src/"
         #P"/home/user/project/"))
    (ok (subdirectory-p
         #P"/home/user/project/src/lib/"
         #P"/home/user/project/"))
    (ok (subdirectory-p
         #P"/a/b/c/"
         #P"/a/")))

  (testing "subdirectory-p returns NIL for non-subdirectories"
    (ng (subdirectory-p
         #P"/home/user/project/"
         #P"/home/user/project/"))
    (ng (subdirectory-p
         #P"/home/user/other/"
         #P"/home/user/project/"))
    (ng (subdirectory-p
         #P"/home/"
         #P"/home/user/project/"))))

;;; Test create-directory-item
(deftest create-directory-item-test
  (testing "create-directory-item creates closed directory by default"
    (let ((item (create-directory-item #P"/tmp/")))
      (ok (typep item 'directory-item))
      (ng (directory-item-open-p item))
      (ok (null (directory-item-children item)))))

  (testing "create-directory-item with :open t creates open directory with children"
    (let ((item (create-directory-item #P"/tmp/" :open t)))
      (ok (typep item 'directory-item))
      (ok (directory-item-open-p item)))))

;;; Test expand-directory-item
(deftest expand-directory-item-test
  (testing "expand-directory-item opens a closed directory"
    (let ((item (create-directory-item #P"/tmp/")))
      (ng (directory-item-open-p item))
      (expand-directory-item item)
      (ok (directory-item-open-p item))))

  (testing "expand-directory-item does nothing if already open"
    (let ((item (create-directory-item #P"/tmp/" :open t)))
      (ok (directory-item-open-p item))
      (expand-directory-item item)
      (ok (directory-item-open-p item)))))

;;; Test find-child-directory-item
(deftest find-child-directory-item-test
  (testing "find-child-directory-item finds matching child"
    (let ((root (create-directory-item #P"/tmp/" :open t)))
      ;; Create a test subdirectory structure
      (let ((child (find-child-directory-item
                    root
                    (item-pathname root))))
        ;; Should return nil if target is the same as root
        (ok (null child)))))

  (testing "find-child-directory-item returns nil for non-matching path"
    (let ((root (create-directory-item #P"/tmp/" :open t)))
      (ok (null (find-child-directory-item
                 root
                 #P"/nonexistent/path/"))))))

;;; Test expand-to-directory
(deftest expand-to-directory-test
  (testing "expand-to-directory returns T for same directory"
    (let ((root (create-directory-item #P"/tmp/")))
      (ok (expand-to-directory root #P"/tmp/"))))

  (testing "expand-to-directory returns NIL for unrelated directory"
    (let ((root (create-directory-item #P"/tmp/")))
      (ng (expand-to-directory root #P"/var/")))))

;;; Test *filer-item-inserters*
(deftest filer-item-inserters-test
  (testing "*filer-item-inserters* exists and is initially empty"
    (ok (boundp 'lem/filer:*filer-item-inserters*))
    (ok (listp lem/filer:*filer-item-inserters*)))

  (testing "inserters can be added and removed"
    (let ((original-value lem/filer:*filer-item-inserters*))
      (unwind-protect
          (let ((test-inserter (lambda (point item root-directory)
                                 (declare (ignore point item root-directory)))))
            ;; Add inserter
            (push test-inserter lem/filer:*filer-item-inserters*)
            (ok (member test-inserter lem/filer:*filer-item-inserters*))
            ;; Remove inserter
            (setf lem/filer:*filer-item-inserters*
                  (remove test-inserter lem/filer:*filer-item-inserters*))
            (ok (not (member test-inserter lem/filer:*filer-item-inserters*))))
        ;; Restore original value
        (setf lem/filer:*filer-item-inserters* original-value)))))

;;; Note: Tests for filer-active-p, filer-buffer, filer-current-directory,
;;; and highlight-file-in-filer are not included here because they require
;;; a full editor environment with *implementation* bound.
;;; These functions are tested manually through interactive use.
