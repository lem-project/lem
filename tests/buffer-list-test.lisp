(defpackage :lem-tests/buffer-list-test
  (:use :cl)
  (:import-from :lem-tests/utilities
                :sample-file)
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

(defmacro with-buffer-list-test ((&optional buffer-list) &body body)
  `(let ((lem-base::*buffer-list* ,buffer-list))
     ,@body))

(rove:deftest buffer-list
  (with-buffer-list-test ()
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
  (with-buffer-list-test ()
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
  (with-buffer-list-test ()
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
                   (lem-base:bufferp buffer)))))
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
  (with-buffer-list-test ()
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
      (with-buffer-list-test ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a<2>)
        (rove:ok (equal "a<2>" (lem-base:unique-buffer-name "a")))
        (rove:ok (equal (lem-base:buffer-list)
                        (list buffer-a<1>
                              buffer-a))))
      (with-buffer-list-test ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a<1>)
        (rove:ok (equal "a<1>" (lem-base:unique-buffer-name "a")))
        (rove:ok (equal (lem-base:buffer-list)
                        (list buffer-a<2>
                              buffer-a))))
      (with-buffer-list-test ((copy-list (lem-base:buffer-list)))
        (lem-base:delete-buffer buffer-a)
        (rove:ok (equal "a" (lem-base:unique-buffer-name "a")))
        (rove:ok (equal (lem-base:buffer-list)
                        (list buffer-a<2>
                              buffer-a<1>))))
      (rove:ok (equal "b" (lem-base:unique-buffer-name "b"))))))

(rove:deftest delete-buffer
  )

(rove:deftest get-next-buffer
  )

(rove:deftest get-previous-buffer
  )

(rove:deftest unbury-buffer
  )

(rove:deftest bury-buffer
  )

(rove:deftest get-file-buffer
  )
