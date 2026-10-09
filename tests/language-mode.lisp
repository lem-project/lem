(defpackage :lem-tests/language-mode
  (:use :cl :rove))
(in-package :lem-tests/language-mode)

(defun filesystem-root (pathname)
  (loop :with directory := (uiop:ensure-directory-pathname pathname)
        :for parent := (uiop:pathname-parent-directory-pathname directory)
        :when (uiop:pathname-equal directory parent)
          :return directory
        :do (setf directory parent)))

(deftest find-root-directory-stops-at-filesystem-root
  (let* ((root (filesystem-root (uiop:getcwd)))
         (entries (lem/buffer/file-utils:list-directory root))
         (entry-count (length entries)))
    (cond
      ((uiop:pathname-equal root (user-homedir-pathname))
       (skip "filesystem root is also the home directory"))
      ((zerop entry-count)
       (skip "filesystem root has no entries"))
      (t
       (let ((calls 0))
         (ok (uiop:pathname-equal
              root
              (lem/language-mode:find-root-directory
               root
               (list (lambda (name)
                       (declare (ignore name))
                       (incf calls)
                       (> calls entry-count))))))
         (ok (= entry-count calls)
             "filesystem root should be scanned only once"))))))

(deftest find-root-directory-checks-marker-before-root-stop
  (let* ((root (filesystem-root (uiop:getcwd)))
         (entries (lem/buffer/file-utils:list-directory root)))
    (if (null entries)
        (skip "filesystem root has no entries")
        (let ((calls 0))
          (ok (uiop:pathname-equal
               root
               (lem/language-mode:find-root-directory
                root
                (list (lambda (name)
                        (declare (ignore name))
                        (incf calls)
                        t)))))
          (ok (= 1 calls)
              "root markers should be checked before stopping at filesystem root")))))
