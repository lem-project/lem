(defpackage :lem-tests/buffer-list-test
  (:use :cl)
  (:import-from :lem-tests/utilities
                :sample-file
                :with-global-variable-value)
  (:import-from :lem-base)
  (:import-from :rove)
  (:import-from :alexandria))
(in-package :lem-tests/buffer-list-test)

#|
- buffer-list
- any-modified-buffer-p
- get-buffer
- unique-buffer-name
- delete-buffer
- get-next-buffer
- get-previous-buffer
- unbruy-buffer
- bury-buffer
- get-file-buffer
|#

(defmacro with-buffer-list ((&optional buffer-list) &body body)
  `(let ((lem-base::*buffer-list* ,buffer-list))
     ,@body))

(rove:deftest buffer-list
  (with-buffer-list ()
    (rove:ok (null (lem-base:buffer-list)))
    (let ((buffer (lem-base:make-buffer "a" :temporary t)))
      (rove:ok (lem-base:bufferp buffer))
      (rove:ok (null (lem-base:buffer-list))))
    (let (buffer-a buffer-b buffer-c)
      (rove:testing "make buffer-a"
        (setf buffer-a (lem-base:make-buffer "a"))
        (rove:ok (equal (list buffer-a) (lem-base:buffer-list))))
      (rove:testing "make buffer-b"
        (setf buffer-b (lem-base:make-buffer "b"))
        (rove:ok (equal (list buffer-b buffer-a)
                        (lem-base:buffer-list))))
      (rove:testing "make buffer-c"
        (setf buffer-c (lem-base:make-buffer "c"))
        (rove:ok (equal (list buffer-c buffer-b buffer-a)
                        (lem-base:buffer-list)))))))

(rove:deftest any-modified-buffer-p
  (with-buffer-list ()
    (let ((buffer-a (lem-base:make-buffer "a"))
          (buffer-b (lem-base:make-buffer "b"))
          (buffer-c (lem-base:find-file-buffer (sample-file "text.txt"))))
      (rove:ok (equal (list buffer-c
                            buffer-b
                            buffer-a)
                      (lem-base:buffer-list)))
      (rove:ok (not (lem-base:any-modified-buffer-p)))
      (rove:testing "edit buffer-a, any-modified-buffer-p = nil"
        (lem-base:with-point ((p (lem-base:buffer-point buffer-a)))
          (lem-base:insert-character p #\a)
          (rove:ok (not (lem-base:any-modified-buffer-p)))))
      (rove:testing "edit buffer-b, any-modified-buffer-p = nil"
        (lem-base:with-point ((p (lem-base:buffer-point buffer-b)))
          (lem-base:insert-character p #\a)
          (rove:ok (not (lem-base:any-modified-buffer-p)))))
      (rove:testing "edit buffer-c, any-modified-buffer-p = t"
        (lem-base:with-point ((p (lem-base:buffer-point buffer-c)))
          (lem-base:insert-character p #\a)
          (rove:ok (eq t (lem-base:any-modified-buffer-p))))))))

(rove:deftest get-buffer
  (with-buffer-list ()
    (rove:testing "arugment type"
      (rove:ok (rove:signals (lem-base:get-buffer 1) 'type-error))
      (rove:ok (rove:signals (lem-base:get-buffer nil) 'type-error))
      (rove:ok (handler-case (lem-base:get-buffer "foo")
                 (error ()
                   nil)
                 (:no-error (buffer)
                   (null buffer))))
      (rove:ok (handler-case (lem-base:get-buffer (lem:make-buffer nil :temporary t))
                 (error ()
                   nil)
                 (:no-error (buffer)
                   (lem-base:bufferp buffer))))))
  (with-buffer-list ()
    (rove:ok (null (lem-base:get-buffer "a")))
    (let (buffer-a buffer-b buffer-c)
      (rove:testing "buffer-a"
        (setf buffer-a (lem-base:make-buffer "a"))
        (rove:ok (eq buffer-a (lem-base:get-buffer "a"))))
      (rove:testing "buffer-b"
        (setf buffer-b (lem-base:make-buffer "b"))
        (rove:ok (eq buffer-a (lem-base:get-buffer "a")))
        (rove:ok (eq buffer-b (lem-base:get-buffer "b"))))
      (rove:testing "buffer-c"
        (setf buffer-c (lem-base:make-buffer "c"))
        (rove:ok (eq buffer-a (lem-base:get-buffer "a")))
        (rove:ok (eq buffer-b (lem-base:get-buffer "b")))
        (rove:ok (eq buffer-c (lem-base:get-buffer "c"))))
      (rove:testing "receive the buffer-object itself"
        (rove:ok (eq buffer-a (lem-base:get-buffer buffer-a)))
        (rove:ok (eq buffer-b (lem-base:get-buffer buffer-b)))
        (rove:ok (eq buffer-c (lem-base:get-buffer buffer-c)))))))

(rove:deftest unique-buffer-name
  (with-buffer-list ()
    (rove:testing "argument type"
      (rove:ok (rove:signals (lem-base:unique-buffer-name (lem-base:make-buffer nil :temporary t)) 'type-error))
      (rove:ok (rove:signals (lem-base:unique-buffer-name 1) 'type-error))
      (rove:ok (rove:signals (lem-base:unique-buffer-name #(100 200)) 'type-error))
      (rove:ok (handler-case (lem-base:unique-buffer-name "abc")
                 (error ()
                   nil)
                 (:no-error (name)
                   (and (stringp name)
                        (string= name "abc")))))))
  (with-buffer-list ()
    (rove:ok (equal "foo" (lem-base:unique-buffer-name "foo")))
    (let ((buffer-a (lem-base:make-buffer "a"))
          buffer-a<1>
          buffer-a<2>)
      (declare (ignorable buffer-a))
      (let ((name (lem-base:unique-buffer-name "a")))
        (rove:ok (equal "a<1>" name))
        (setf buffer-a<1> (lem-base:make-buffer name)))
      (let ((name (lem-base:unique-buffer-name "a")))
        (rove:ok (equal "a<2>" name))
        (setf buffer-a<2> (lem-base:make-buffer name)))
      (rove:ok (string= (lem-base:buffer-name buffer-a) "a"))
      (rove:ok (string= (lem-base:buffer-name buffer-a<1>) "a<1>"))
      (rove:ok (string= (lem-base:buffer-name buffer-a<2>) "a<2>"))
      (rove:ok (equal (lem-base:buffer-list)
                      (list buffer-a<2>
                            buffer-a<1>
                            buffer-a)))
      (with-buffer-list ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a<2>)
        (rove:ok (equal "a<2>" (lem-base:unique-buffer-name "a")))
        (rove:ok (equal (lem-base:buffer-list)
                        (list buffer-a<1>
                              buffer-a))))
      (with-buffer-list ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a<1>)
        (rove:ok (equal "a<1>" (lem-base:unique-buffer-name "a")))
        (rove:ok (equal (lem-base:buffer-list)
                        (list buffer-a<2>
                              buffer-a))))
      (with-buffer-list ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a)
        (rove:ok (equal "a" (lem-base:unique-buffer-name "a")))
        (rove:ok (equal (lem-base:buffer-list)
                        (list buffer-a<2>
                              buffer-a<1>))))
      (rove:ok (equal "b" (lem-base:unique-buffer-name "b"))))))

(rove:deftest delete-buffer
  (with-buffer-list ()
    (rove:testing "argument type"
      (rove:ok (rove:signals (lem-base:delete-buffer 1) 'type-error))
      (rove:ok (rove:signals (lem-base:delete-buffer "name") 'type-error))
      (rove:ok (handler-case (lem-base:delete-buffer (lem-base:make-buffer nil :temporary t))
                 (error ()
                   nil)
                 (:no-error (result)
                   (declare (ignore result))
                   t)))))
  (with-buffer-list ()
    (let ((buffer-a (lem-base:make-buffer "a"))
          (buffer-b (lem-base:make-buffer "b"))
          (buffer-c (lem-base:make-buffer "c")))
      (assert (equal (list buffer-c buffer-b buffer-a)
                     (lem-base:buffer-list)))
      (flet ((test (buffer-list deleting-buffer expected-buffer-list)
               (with-buffer-list ((copy-list buffer-list))
                 (rove:ok (not (lem-base:deleted-buffer-p deleting-buffer)))
                 (let ((result (lem-base:delete-buffer deleting-buffer)))
                   (rove:ok (lem-base:deleted-buffer-p deleting-buffer))
                   (rove:ok (equal result expected-buffer-list))))))
        (test (lem-base:buffer-list) buffer-a (list buffer-c buffer-b))
        (test (lem-base:buffer-list) buffer-b (list buffer-c buffer-a))
        (test (lem-base:buffer-list) buffer-c (list buffer-b buffer-a))))
    (rove:testing "temporary buffer"
      (let ((buffer (lem-base:make-buffer nil :temporary t))
            (buffer-list (copy-list (lem-base:buffer-list))))
        (rove:ok (not (lem-base:deleted-buffer-p buffer)))
        (rove:ok (equal buffer-list (lem-base:delete-buffer buffer)))
        (rove:ok (lem-base:deleted-buffer-p buffer))))
    (rove:testing "kill-buffer-hook"
      (flet ((hook-body (hooked-buffer deleting-buffer)
               (rove:ok (eq hooked-buffer deleting-buffer))
               (rove:ok (not (lem-base:deleted-buffer-p hooked-buffer)))))
        (rove:testing "buffer local"
          (let ((buffer (lem-base:make-buffer "test"))
                (called-hook-p nil))
            (flet ((hook (arg)
                     (setf called-hook-p t)
                     (hook-body arg buffer)))
              (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :buffer buffer)
                                 #'hook)
              (lem-base:delete-buffer buffer)
              (rove:ok called-hook-p))))
        (rove:testing "global"
          (with-global-variable-value (lem-base:kill-buffer-hook nil)
            (let ((buffer (lem-base:make-buffer "test"))
                  (called-hook-p nil))
              (flet ((hook (arg)
                       (setf called-hook-p t)
                       (hook-body arg buffer)))
                (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :global)
                                   #'hook)
                (lem-base:delete-buffer buffer)
                (rove:ok called-hook-p)))))
        (rove:testing "local/global"
          (with-global-variable-value (lem-base:kill-buffer-hook nil)
            (let ((buffer (lem-base:make-buffer "test"))
                  (called-order '()))
              (flet ((local-hook (arg)
                       (rove:testing "called local hook"
                         (hook-body arg buffer)
                         (push :local called-order)))
                     (global-hook (arg)
                       (rove:testing "called global hook"
                         (hook-body arg buffer)
                         (push :global called-order))))
                (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :buffer buffer)
                                   #'local-hook)
                (lem-base:add-hook (lem-base:variable-value 'lem-base:kill-buffer-hook :global)
                                   #'global-hook)
                (lem-base:delete-buffer buffer)
                (rove:ok (equal '(:local :global)
                                (nreverse called-order)))))))))))


(flet ((argument-type-test (function)
         (with-buffer-list ()
           (rove:testing "argument type"
             (rove:ok (rove:signals (funcall function nil) 'type-error))
             (rove:ok (rove:signals (funcall function 1) 'type-error))
             (rove:ok (rove:signals (funcall function "name") 'type-error))
             (rove:ok (rove:signals (funcall function #(#\a #\b)) 'type-error)))))
       (buffer-list-length=0-case (function)
         (with-buffer-list ()
           (assert (null (lem-base:buffer-list)))
           (rove:ok (eq (funcall function (lem-base:make-buffer nil :temporary t))
                        nil))))
       (buffer-list-length=1-case (function)
         (with-buffer-list ()
           (let ((buffer-a (lem-base:make-buffer "a")))
             (assert (equal (lem-base:buffer-list)
                            (list buffer-a)))
             (rove:ok (eq (funcall function buffer-a) nil))))))

  (rove:deftest get-next-buffer
    (argument-type-test #'lem-base:get-next-buffer)
    (buffer-list-length=0-case #'lem-base:get-next-buffer)
    (buffer-list-length=1-case #'lem-base:get-next-buffer)
    (with-buffer-list ()
      (let ((buffer-a (lem-base:make-buffer "a"))
            (buffer-b (lem-base:make-buffer "b"))
            (buffer-c (lem-base:make-buffer "c")))
        (assert (equal (lem-base:buffer-list)
                       (list buffer-c buffer-b buffer-a)))
        (rove:ok (eq (lem-base:get-next-buffer buffer-c) buffer-b))
        (rove:ok (eq (lem-base:get-next-buffer buffer-b) buffer-a))
        (rove:ok (eq (lem-base:get-next-buffer buffer-a) nil))
        (rove:ok (eq (lem-base:get-next-buffer (lem-base:make-buffer nil :temporary t)) nil)))))

  (rove:deftest get-previous-buffer
    (argument-type-test #'lem-base:get-previous-buffer)
    (buffer-list-length=0-case #'lem-base:get-previous-buffer)
    (buffer-list-length=1-case #'lem-base:get-previous-buffer)
    (with-buffer-list ()
      (let ((buffer-a (lem-base:make-buffer "a"))
            (buffer-b (lem-base:make-buffer "b"))
            (buffer-c (lem-base:make-buffer "c")))
        (assert (equal (lem-base:buffer-list)
                       (list buffer-c buffer-b buffer-a)))
        (rove:ok (eq (lem-base:get-previous-buffer buffer-c) nil))
        (rove:ok (eq (lem-base:get-previous-buffer buffer-b) buffer-c))
        (rove:ok (eq (lem-base:get-previous-buffer buffer-a) buffer-b))
        (rove:ok (eq (lem-base:get-previous-buffer (lem-base:make-buffer nil :temporary t)) nil))))))

(rove:deftest unbury-buffer
  )

(rove:deftest bury-buffer
  )

(rove:deftest get-file-buffer
  )
