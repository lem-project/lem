(defpackage :lem-tests/filer
  (:use :cl :rove :lem))
(in-package :lem-tests/filer)

;;; Test subdirectory-p
(deftest subdirectory-p-test
  (testing "subdirectory-p returns T for valid subdirectories"
    (ok (lem/filer::subdirectory-p
         #P"/home/user/project/src/"
         #P"/home/user/project/"))
    (ok (lem/filer::subdirectory-p
         #P"/home/user/project/src/lib/"
         #P"/home/user/project/"))
    (ok (lem/filer::subdirectory-p
         #P"/a/b/c/"
         #P"/a/")))

  (testing "subdirectory-p returns NIL for non-subdirectories"
    (ng (lem/filer::subdirectory-p
         #P"/home/user/project/"
         #P"/home/user/project/"))
    (ng (lem/filer::subdirectory-p
         #P"/home/user/other/"
         #P"/home/user/project/"))
    (ng (lem/filer::subdirectory-p
         #P"/home/"
         #P"/home/user/project/"))))

;;; Test create-directory-item
(deftest create-directory-item-test
  (testing "create-directory-item creates closed directory by default"
    (let ((item (lem/filer::create-directory-item #P"/tmp/")))
      (ok (typep item 'lem/filer::directory-item))
      (ng (lem/filer::directory-item-open-p item))
      (ok (null (lem/filer::directory-item-children item)))))

  (testing "create-directory-item with :open t creates open directory with children"
    (let ((item (lem/filer::create-directory-item #P"/tmp/" :open t)))
      (ok (typep item 'lem/filer::directory-item))
      (ok (lem/filer::directory-item-open-p item)))))

;;; Test expand-directory-item
(deftest expand-directory-item-test
  (testing "expand-directory-item opens a closed directory"
    (let ((item (lem/filer::create-directory-item #P"/tmp/")))
      (ng (lem/filer::directory-item-open-p item))
      (lem/filer::expand-directory-item item)
      (ok (lem/filer::directory-item-open-p item))))

  (testing "expand-directory-item does nothing if already open"
    (let ((item (lem/filer::create-directory-item #P"/tmp/" :open t)))
      (ok (lem/filer::directory-item-open-p item))
      (lem/filer::expand-directory-item item)
      (ok (lem/filer::directory-item-open-p item)))))

;;; Test find-child-directory-item
(deftest find-child-directory-item-test
  (testing "find-child-directory-item finds matching child"
    (let ((root (lem/filer::create-directory-item #P"/tmp/" :open t)))
      ;; Create a test subdirectory structure
      (let ((child (lem/filer::find-child-directory-item
                    root
                    (lem/filer::item-pathname root))))
        ;; Should return nil if target is the same as root
        (ok (null child)))))

  (testing "find-child-directory-item returns nil for non-matching path"
    (let ((root (lem/filer::create-directory-item #P"/tmp/" :open t)))
      (ok (null (lem/filer::find-child-directory-item
                 root
                 #P"/nonexistent/path/"))))))

;;; Test expand-to-directory
(deftest expand-to-directory-test
  (testing "expand-to-directory returns T for same directory"
    (let ((root (lem/filer::create-directory-item #P"/tmp/")))
      (ok (lem/filer::expand-to-directory root #P"/tmp/"))))

  (testing "expand-to-directory returns NIL for unrelated directory"
    (let ((root (lem/filer::create-directory-item #P"/tmp/")))
      (ng (lem/filer::expand-to-directory root #P"/var/")))))

;;; Note: Tests for filer-active-p, filer-buffer, filer-current-directory,
;;; and highlight-file-in-filer are not included here because they require
;;; a full editor environment with *implementation* bound.
;;; These functions are tested manually through interactive use.
