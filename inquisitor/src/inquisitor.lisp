(in-package :cl-user)
(defpackage inquisitor
  (:nicknames :inq)
  (:use :cl)
  (:import-from :inquisitor.encoding.guess
                :ces-guess-from-vector
                :list-available-scheme)
  (:import-from :inquisitor.encoding.keyword
                :utf8-keyword
                :ucs-2le-keyword
                :ucs-2be-keyword
                :utf16-keyword)
  (:import-from :inquisitor.eol
                :eol-available-p
                :eol-guess-from-vector)
  (:import-from :inquisitor.util
                :with-byte-array
                :byte-array-p
                :byte-input-stream-p
                :file-position-changable-p)
  (:import-from :metabang-bind
                :bind)
  (:export :*detecting-buffer-size*
           :unicode-p
           :make-external-format
           :list-available-scheme
           :detect-encoding
           :detect-end-of-line
           :detect-external-format
           :detect-external-format-from-file))
(in-package :inquisitor)


(defparameter *detecting-buffer-size* 1000)


(defun unicode-p (encoding)
  (member encoding
          (list (utf8-keyword)
                (ucs-2le-keyword)
                (ucs-2be-keyword)
                (utf16-keyword))))

(defun make-external-format (enc eol)
  #+clisp (ext:make-encoding :charset enc
                             :line-terminator eol)
  #+ecl `(,enc ,eol)
  #+sbcl enc
  #+ccl (ccl:make-external-format :character-encoding enc
                                  :line-termination eol)
  #+abcl `(,enc :eol-style ,eol)
  #-(or clisp ecl sbcl ccl abcl) (error "your implementation is not supported."))


(defmethod detect-encoding ((stream stream) (scheme symbol))
  (if (byte-input-stream-p stream)
      (if (file-position-changable-p stream)
          (let ((pos (file-position stream)))
            (with-byte-array (vec *detecting-buffer-size*)
              (read-sequence vec stream)
              (prog1
                  (ces-guess-from-vector vec scheme)
                (file-position stream pos))))
          (error (format nil "supplied stream is not file-position changable.")))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-encoding ((path pathname) (scheme symbol))
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-encoding in scheme)))

(defmethod detect-end-of-line ((stream stream))
  (if (byte-input-stream-p stream)
      (if (file-position-changable-p stream)
          (let ((pos (file-position stream)))
            (with-byte-array (vec *detecting-buffer-size*)
              (read-sequence vec stream)
              (prog1
                  (eol-guess-from-vector vec)
                (file-position stream pos))))
          (error (format nil "supplied stream is not file-position changable.")))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-end-of-line ((path pathname))
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-end-of-line in)))

(defmethod detect-external-format ((vec vector) (scheme symbol))
  (if (byte-array-p vec)
      (bind (((:values enc enc-ct) (ces-guess-from-vector vec scheme))
             ((:values eol eol-ct) (eol-guess-from-vector vec)))
        (if enc-ct
            (error (format nil "unsupported on ~a: ~{~a~^, ~}"
                           (lisp-implementation-type) enc))
            (make-external-format enc eol)))
      (error (format nil "supplied vector is not a byte array."))))

(defmethod detect-external-format ((stream stream) (scheme symbol))
  (if (byte-input-stream-p stream)
      (if (file-position-changable-p stream)
          (let ((pos (file-position stream)))
            (with-byte-array (vec *detecting-buffer-size*)
              (read-sequence vec stream)
              (prog1
                  (detect-external-format vec scheme)
                (file-position stream pos))))
          (error (format nil "supplied stream is not file-position changable.")))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-external-format ((path pathname) (scheme symbol))
  (detect-external-format-from-file path scheme nil))

(defun detect-external-format-from-file (path scheme &optional all-scan-p)
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (if all-scan-p
        (let ((*detecting-buffer-size* (file-length in)))
          (detect-external-format in scheme))
        (detect-external-format in scheme))))
