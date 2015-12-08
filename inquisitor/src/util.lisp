(in-package :cl-user)
(defpackage inquisitor.util
  (:use :cl)
  (:export :with-byte-array
           :byte-array-p
           :byte-input-stream-p
           :file-position-changable-p)
  (:import-from :alexandria
                :type=))
(in-package :inquisitor.util)


(defmacro with-byte-array ((var dim) &body body)
  `(let ((,var (make-array ,dim :element-type '(unsigned-byte 8))))
     ,@body))

(defun byte-array-p (vec)
  (and (typep vec 'vector)
       (type= (array-element-type vec) '(unsigned-byte 8))))

(defun byte-input-stream-p (stream)
  (and (typep stream 'stream)
       (input-stream-p stream)
       (type= (stream-element-type stream) '(unsigned-byte 8))))

(defun file-position-changable-p (stream)
  (and (typep stream 'stream)
       (input-stream-p stream)
       (let ((pos (file-position stream)))
         (prog1
             (and (not (null pos))
                  (file-position stream pos))
           (file-position stream pos)))))
