(defpackage :lem-tests/buffer-list-test
  (:use :cl
        :rove)
  (:import-from :lem-tests/utilities
                :sample-file)
  (:import-from :lem-base
                :with-global-variable-value)
  (:import-from :alexandria))
(in-package :lem-tests/buffer-list-test)

(defmacro with-buffer-list ((&optional buffer-list) &body body)
  `(lem-base::with-current-buffers ,buffer-list ,@body))

(defun argument-type-is-buffer-test (function &key allow-string-p)
  (with-buffer-list ()
    (testing "argument type"
      (ok (signals (funcall function nil) 'type-error))
      (ok (signals (funcall function 1) 'type-error))
      (ok (signals (funcall function #(#\a #\b)) 'type-error))
      (unless allow-string-p
        (ok (signals (funcall function "name") 'type-error))))))

(deftest buffer-list
  (with-buffer-list ()
    (ok (null (lem-base:buffer-list)))
    (let ((buffer (lem-base:make-buffer "a" :temporary t)))
      (ok (lem-base:bufferp buffer))
      (ok (null (lem-base:buffer-list))))
    (let (buffer-a buffer-b buffer-c)
      (testing "make buffer-a"
        (setf buffer-a (lem-base:make-buffer "a"))
        (ok (equal (list buffer-a) (lem-base:buffer-list))))
      (testing "make buffer-b"
        (setf buffer-b (lem-base:make-buffer "b"))
        (ok (equal (list buffer-b buffer-a)
                        (lem-base:buffer-list))))
      (testing "make buffer-c"
        (setf buffer-c (lem-base:make-buffer "c"))
        (ok (equal (list buffer-c buffer-b buffer-a)
                        (lem-base:buffer-list)))))))

(deftest any-modified-buffer-p
  (with-buffer-list ()
    (let ((buffer-a (lem-base:make-buffer "a"))
          (buffer-b (lem-base:make-buffer "b"))
          (buffer-c (lem-base:find-file-buffer (sample-file "text.txt"))))
      (ok (equal (list buffer-c
                            buffer-b
                            buffer-a)
                      (lem-base:buffer-list)))
      (ok (not (lem-base:any-modified-buffer-p)))
      (testing "edit buffer-a, any-modified-buffer-p = nil"
        (lem-base:with-point ((p (lem-base:buffer-point buffer-a)))
          (lem-base:insert-character p #\a)
          (ok (not (lem-base:any-modified-buffer-p)))))
      (testing "edit buffer-b, any-modified-buffer-p = nil"
        (lem-base:with-point ((p (lem-base:buffer-point buffer-b)))
          (lem-base:insert-character p #\a)
          (ok (not (lem-base:any-modified-buffer-p)))))
      (testing "edit buffer-c, any-modified-buffer-p = t"
        (lem-base:with-point ((p (lem-base:buffer-point buffer-c)))
          (lem-base:insert-character p #\a)
          (ok (eq t (lem-base:any-modified-buffer-p))))))))

(deftest get-buffer
  (argument-type-is-buffer-test #'lem-base:get-buffer :allow-string-p t)
  (with-buffer-list ()
    (ok (null (lem-base:get-buffer "a")))
    (let (buffer-a buffer-b buffer-c)
      (testing "buffer-a"
        (setf buffer-a (lem-base:make-buffer "a"))
        (ok (eq buffer-a (lem-base:get-buffer "a"))))
      (testing "buffer-b"
        (setf buffer-b (lem-base:make-buffer "b"))
        (ok (eq buffer-a (lem-base:get-buffer "a")))
        (ok (eq buffer-b (lem-base:get-buffer "b"))))
      (testing "buffer-c"
        (setf buffer-c (lem-base:make-buffer "c"))
        (ok (eq buffer-a (lem-base:get-buffer "a")))
        (ok (eq buffer-b (lem-base:get-buffer "b")))
        (ok (eq buffer-c (lem-base:get-buffer "c"))))
      (testing "receive the buffer-object itself"
        (ok (eq buffer-a (lem-base:get-buffer buffer-a)))
        (ok (eq buffer-b (lem-base:get-buffer buffer-b)))
        (ok (eq buffer-c (lem-base:get-buffer buffer-c)))))))

(deftest unique-buffer-name
  (argument-type-is-buffer-test #'lem-base:unique-buffer-name :allow-string-p t)
  (with-buffer-list ()
    (ok (equal "foo" (lem-base:unique-buffer-name "foo")))
    (let ((buffer-a (lem-base:make-buffer "a"))
          buffer-a<1>
          buffer-a<2>)
      (declare (ignorable buffer-a))
      (let ((name (lem-base:unique-buffer-name "a")))
        (ok (equal "a<1>" name))
        (setf buffer-a<1> (lem-base:make-buffer name)))
      (let ((name (lem-base:unique-buffer-name "a")))
        (ok (equal "a<2>" name))
        (setf buffer-a<2> (lem-base:make-buffer name)))
      (ok (string= (lem-base:buffer-name buffer-a) "a"))
      (ok (string= (lem-base:buffer-name buffer-a<1>) "a<1>"))
      (ok (string= (lem-base:buffer-name buffer-a<2>) "a<2>"))
      (ok (equal (lem-base:buffer-list)
                      (list buffer-a<2>
                            buffer-a<1>
                            buffer-a)))
      (with-buffer-list ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a<2>)
        (ok (equal "a<2>" (lem-base:unique-buffer-name "a")))
        (ok (equal (lem-base:buffer-list)
                        (list buffer-a<1>
                              buffer-a))))
      (with-buffer-list ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a<1>)
        (ok (equal "a<1>" (lem-base:unique-buffer-name "a")))
        (ok (equal (lem-base:buffer-list)
                        (list buffer-a<2>
                              buffer-a))))
      (with-buffer-list ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a)
        (ok (equal "a" (lem-base:unique-buffer-name "a")))
        (ok (equal (lem-base:buffer-list)
                        (list buffer-a<2>
                              buffer-a<1>))))
      (ok (equal "b" (lem-base:unique-buffer-name "b"))))))

(deftest delete-buffer
  (argument-type-is-buffer-test #'lem-base:delete-buffer)
  (with-buffer-list ()
    (let ((buffer-a (lem-base:make-buffer "a"))
          (buffer-b (lem-base:make-buffer "b"))
          (buffer-c (lem-base:make-buffer "c")))
      (assert (equal (list buffer-c buffer-b buffer-a)
                     (lem-base:buffer-list)))
      (flet ((test (buffer-list deleting-buffer expected-buffer-list)
               (with-buffer-list ((copy-list buffer-list))
                 (ok (not (lem-base:deleted-buffer-p deleting-buffer)))
                 (let ((result (lem-base:delete-buffer deleting-buffer)))
                   (ok (lem-base:deleted-buffer-p deleting-buffer))
                   (ok (equal result expected-buffer-list))))))
        (test (lem-base:buffer-list) buffer-a (list buffer-c buffer-b))
        (test (lem-base:buffer-list) buffer-b (list buffer-c buffer-a))
        (test (lem-base:buffer-list) buffer-c (list buffer-b buffer-a))))
    (testing "temporary buffer"
      (let ((buffer (lem-base:make-buffer nil :temporary t))
            (buffer-list (copy-list (lem-base:buffer-list))))
        (ok (not (lem-base:deleted-buffer-p buffer)))
        (ok (equal buffer-list (lem-base:delete-buffer buffer)))
        (ok (lem-base:deleted-buffer-p buffer))))
    (testing "kill-buffer-hook"
      (flet ((hook-body (hooked-buffer deleting-buffer)
               (ok (eq hooked-buffer deleting-buffer))
               (ok (not (lem-base:deleted-buffer-p hooked-buffer)))))
        (testing "buffer local"
          (let ((buffer (lem-base:make-buffer "test"))
                (called-hook-p nil))
            (flet ((hook (arg)
                     (setf called-hook-p t)
                     (hook-body arg buffer)))
              (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :buffer buffer)
                                 #'hook)
              (lem-base:delete-buffer buffer)
              (ok called-hook-p))))
        (testing "global"
          (with-global-variable-value (lem-base:kill-buffer-hook nil)
            (let ((buffer (lem-base:make-buffer "test"))
                  (called-hook-p nil))
              (flet ((hook (arg)
                       (setf called-hook-p t)
                       (hook-body arg buffer)))
                (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :global)
                                   #'hook)
                (lem-base:delete-buffer buffer)
                (ok called-hook-p)))))
        (testing "local/global"
          (with-global-variable-value (lem-base:kill-buffer-hook nil)
            (let ((buffer (lem-base:make-buffer "test"))
                  (called-order '()))
              (flet ((local-hook (arg)
                       (testing "called local hook"
                         (hook-body arg buffer)
                         (push :local called-order)))
                     (global-hook (arg)
                       (testing "called global hook"
                         (hook-body arg buffer)
                         (push :global called-order))))
                (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :buffer buffer)
                                   #'local-hook)
                (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :global)
                                   #'global-hook)
                (lem-base:delete-buffer buffer)
                (ok (equal '(:local :global)
                                (nreverse called-order)))))))))))

(flet ((buffer-list-length=0-case (function)
         (testing "buffer-list length is 0"
           (with-buffer-list ()
             (assert (null (lem-base:buffer-list)))
             (ok (eq (funcall function (lem-base:make-buffer nil :temporary t))
                          nil)))))
       (buffer-list-length=1-case (function)
         (testing "buffer-list length is 1"
           (with-buffer-list ()
             (let ((buffer-a (lem-base:make-buffer "a")))
               (assert (equal (lem-base:buffer-list)
                              (list buffer-a)))
               (ok (eq (funcall function buffer-a) nil)))))))

  (deftest get-next-buffer
    (argument-type-is-buffer-test #'lem-base:get-next-buffer)
    (buffer-list-length=0-case #'lem-base:get-next-buffer)
    (buffer-list-length=1-case #'lem-base:get-next-buffer)
    (testing "buffer-list length is 3"
      (with-buffer-list ()
        (let ((buffer-a (lem-base:make-buffer "a"))
              (buffer-b (lem-base:make-buffer "b"))
              (buffer-c (lem-base:make-buffer "c")))
          (assert (equal (lem-base:buffer-list)
                         (list buffer-c buffer-b buffer-a)))
          (ok (eq (lem-base:get-next-buffer buffer-c) buffer-b))
          (ok (eq (lem-base:get-next-buffer buffer-b) buffer-a))
          (ok (eq (lem-base:get-next-buffer buffer-a) nil))
          (ok (eq (lem-base:get-next-buffer (lem-base:make-buffer nil :temporary t)) nil))))))

  (deftest get-previous-buffer
    (argument-type-is-buffer-test #'lem-base:get-previous-buffer)
    (buffer-list-length=0-case #'lem-base:get-previous-buffer)
    (buffer-list-length=1-case #'lem-base:get-previous-buffer)
    (testing "buffer-list length is 3"
      (with-buffer-list ()
        (let ((buffer-a (lem-base:make-buffer "a"))
              (buffer-b (lem-base:make-buffer "b"))
              (buffer-c (lem-base:make-buffer "c")))
          (assert (equal (lem-base:buffer-list)
                         (list buffer-c buffer-b buffer-a)))
          (ok (eq (lem-base:get-previous-buffer buffer-c) nil))
          (ok (eq (lem-base:get-previous-buffer buffer-b) buffer-c))
          (ok (eq (lem-base:get-previous-buffer buffer-a) buffer-b))
          (ok (eq (lem-base:get-previous-buffer (lem-base:make-buffer nil :temporary t)) nil)))))))

(deftest bury-buffer
  (argument-type-is-buffer-test #'lem-base:bury-buffer)
  (testing "buffer-list length is 1"
    (with-buffer-list ()
      (let ((buffer-a (lem-base:make-buffer "a")))
        (assert (equal (lem-base:buffer-list)
                       (list buffer-a)))
        (ok (eq buffer-a (lem-base:bury-buffer buffer-a)))
        (ok (equal (lem-base:buffer-list)
                        (list buffer-a))))))
  (testing "buffer-list length is 3"
    (with-buffer-list ()
      (let ((buffer-a (lem-base:make-buffer "a"))
            (buffer-b (lem-base:make-buffer "b"))
            (buffer-c (lem-base:make-buffer "c")))
        (assert (equal (lem-base:buffer-list)
                       (list buffer-c buffer-b buffer-a)))
        (with-buffer-list ((copy-list (lem-base:buffer-list)))
          (ok (eq buffer-b (lem-base:bury-buffer buffer-c)))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-b buffer-a buffer-c))))
        (with-buffer-list ((copy-list (lem-base:buffer-list)))
          (ok (eq buffer-c (lem-base:bury-buffer buffer-b)))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-c buffer-a buffer-b))))
        (with-buffer-list ((copy-list (lem-base:buffer-list)))
          (ok (eq buffer-c (lem-base:bury-buffer buffer-a)))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-c buffer-b buffer-a)))))))
  (testing "temporary buffer"
    (testing "buffer-list length is 0"
      (with-buffer-list ()
        (assert (null (lem-base:buffer-list)))
        (ok (eq nil (lem-base:bury-buffer (lem-base:make-buffer nil :temporary t))))))
    (testing "buffer-list length is 3"
      (with-buffer-list ()
        (let ((buffer-a (lem-base:make-buffer "a"))
              (buffer-b (lem-base:make-buffer "b"))
              (buffer-c (lem-base:make-buffer "c")))
          (assert (equal (lem-base:buffer-list)
                         (list buffer-c buffer-b buffer-a)))
          (ok (eq buffer-c (lem-base:bury-buffer (lem-base:make-buffer nil :temporary t))))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-c buffer-b buffer-a))))))))

(deftest unbury-buffer
  (argument-type-is-buffer-test #'lem-base:unbury-buffer)
  (testing "buffer-list length is 1"
    (with-buffer-list ()
      (let ((buffer-a (lem-base:make-buffer "a")))
        (assert (equal (lem-base:buffer-list)
                       (list buffer-a)))
        (ok (eq buffer-a (lem-base:unbury-buffer buffer-a)))
        (ok (equal (lem-base:buffer-list)
                        (list buffer-a))))))
  (testing "buffer-list length is 3"
    (with-buffer-list ()
      (let ((buffer-a (lem-base:make-buffer "a"))
            (buffer-b (lem-base:make-buffer "b"))
            (buffer-c (lem-base:make-buffer "c")))
        (assert (equal (lem-base:buffer-list)
                       (list buffer-c buffer-b buffer-a)))
        (with-buffer-list ((copy-list (lem-base:buffer-list)))
          (ok (eq buffer-a (lem-base:unbury-buffer buffer-a)))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-a buffer-c buffer-b))))
        (with-buffer-list ((copy-list (lem-base:buffer-list)))
          (ok (eq buffer-b (lem-base:unbury-buffer buffer-b)))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-b buffer-c buffer-a))))
        (with-buffer-list ((copy-list (lem-base:buffer-list)))
          (ok (eq buffer-c (lem-base:unbury-buffer buffer-c)))
          (ok (equal (lem-base:buffer-list)
                          (list buffer-c buffer-b buffer-a)))))))
  (testing "temporary buffer"
    (testing "buffer-list length is 0"
      (with-buffer-list ()
        (assert (null (lem-base:buffer-list)))
        (let ((buffer (lem-base:make-buffer nil :temporary t)))
          (ok (eq buffer (lem-base:unbury-buffer buffer))))))
    (testing "buffer-list length is 3"
      (with-buffer-list ()
        (let ((buffer-a (lem-base:make-buffer "a"))
              (buffer-b (lem-base:make-buffer "b"))
              (buffer-c (lem-base:make-buffer "c")))
          (assert (equal (lem-base:buffer-list)
                         (list buffer-c buffer-b buffer-a)))
          (let ((buffer (lem-base:make-buffer nil :temporary t)))
            (ok (eq buffer (lem-base:unbury-buffer buffer)))
            (ok (equal (lem-base:buffer-list)
                            (list buffer-c buffer-b buffer-a)))))))))

(deftest get-file-buffer
  (testing "argument type"
    (ok (signals (lem-base:get-file-buffer nil) 'type-error))
    (ok (signals (lem-base:get-file-buffer t) 'type-error))
    (ok (signals (lem-base:get-file-buffer 1) 'type-error))
    (ok (signals (lem-base:get-file-buffer #(#\a)) 'type-error)))
  (with-buffer-list ()
    (let ((filename (sample-file "test.txt")))
      (lem-base:make-buffer "a")
      (lem-base:make-buffer "b")
      (lem-base:make-buffer "c")
      (ok (null (lem-base:get-file-buffer filename)))
      (let ((buffer (lem-base:find-file-buffer filename)))
        (ok (eq (lem-base:get-file-buffer filename)
                     buffer))))))
