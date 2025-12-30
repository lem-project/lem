(defpackage :lem-clojure-mode/bencode
  (:use :cl)
  (:export :bencode-encode
           :bencode-decode
           :bencode-decode-from-stream
           :bencode-error))

(in-package :lem-clojure-mode/bencode)

;;;; Bencode Implementation for nREPL Protocol
;;;;
;;;; Bencode is a simple encoding format used by BitTorrent and nREPL.
;;;; Types:
;;;;   - Integers: i<number>e (e.g., i42e)
;;;;   - Strings: <length>:<contents> (e.g., 4:spam)
;;;;   - Lists: l<contents>e (e.g., l4:spami42ee)
;;;;   - Dictionaries: d<contents>e (e.g., d3:foo3:bare)

(define-condition bencode-error (error)
  ((message :initarg :message :reader bencode-error-message))
  (:report (lambda (condition stream)
             (format stream "Bencode error: ~A" (bencode-error-message condition)))))

;;;; Encoding

(defgeneric bencode-encode (value)
  (:documentation "Encode VALUE to bencode format, returning a string."))

(defmethod bencode-encode ((value integer))
  "Encode an integer: i<number>e"
  (format nil "i~De" value))

(defmethod bencode-encode ((value string))
  "Encode a string: <length>:<contents>"
  (let ((bytes (babel:string-to-octets value :encoding :utf-8)))
    (format nil "~D:~A" (length bytes) value)))

(defmethod bencode-encode ((value symbol))
  "Encode a symbol as a string (using its name)."
  (bencode-encode (string-downcase (symbol-name value))))

(defmethod bencode-encode ((value list))
  "Encode a list: l<contents>e"
  (with-output-to-string (out)
    (write-char #\l out)
    (dolist (item value)
      (write-string (bencode-encode item) out))
    (write-char #\e out)))

(defmethod bencode-encode ((value hash-table))
  "Encode a hash-table as dictionary: d<key><value>...e
   Keys must be strings and are sorted lexicographically."
  (with-output-to-string (out)
    (write-char #\d out)
    (let ((keys (sort (loop :for key :being :the :hash-keys :of value
                            :collect key)
                      #'string<)))
      (dolist (key keys)
        (write-string (bencode-encode key) out)
        (write-string (bencode-encode (gethash key value)) out)))
    (write-char #\e out)))

(defun make-nrepl-message (&rest pairs)
  "Create an nREPL message as a hash-table from key-value pairs."
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on pairs :by #'cddr
          :do (setf (gethash (if (keywordp key)
                                 (string-downcase (symbol-name key))
                                 key)
                             table)
                    value))
    table))

;;;; Decoding

(defun bencode-decode (string)
  "Decode a bencode string, returning the decoded value."
  (with-input-from-string (stream string)
    (bencode-decode-from-stream stream)))

(defun bencode-decode-from-stream (stream)
  "Decode bencode from a stream."
  (let ((char (peek-char nil stream nil nil)))
    (cond
      ((null char)
       (error 'bencode-error :message "Unexpected end of stream"))
      ((char= char #\i)
       (decode-integer stream))
      ((char= char #\l)
       (decode-list stream))
      ((char= char #\d)
       (decode-dictionary stream))
      ((digit-char-p char)
       (decode-string stream))
      (t
       (error 'bencode-error :message (format nil "Unexpected character: ~C" char))))))

(defun decode-integer (stream)
  "Decode an integer: i<number>e"
  (read-char stream)  ; consume 'i'
  (let ((number-string (with-output-to-string (out)
                         (loop :for char := (read-char stream)
                               :until (char= char #\e)
                               :do (write-char char out)))))
    (parse-integer number-string)))

(defun decode-string (stream)
  "Decode a string: <length>:<contents>"
  (let* ((length-string (with-output-to-string (out)
                          (loop :for char := (read-char stream)
                                :until (char= char #\:)
                                :do (write-char char out))))
         (length (parse-integer length-string))
         (buffer (make-array length :element-type 'character)))
    (read-sequence buffer stream)
    buffer))

(defun decode-list (stream)
  "Decode a list: l<contents>e"
  (read-char stream)  ; consume 'l'
  (loop :until (char= (peek-char nil stream) #\e)
        :collect (bencode-decode-from-stream stream)
        :finally (read-char stream)))  ; consume 'e'

(defun decode-dictionary (stream)
  "Decode a dictionary: d<key><value>...e"
  (read-char stream)  ; consume 'd'
  (let ((table (make-hash-table :test 'equal)))
    (loop :until (char= (peek-char nil stream) #\e)
          :do (let ((key (bencode-decode-from-stream stream))
                    (value (bencode-decode-from-stream stream)))
                (setf (gethash key table) value))
          :finally (read-char stream))  ; consume 'e'
    table))

;;;; Utility Functions

(defun bencode-encode-to-octets (value)
  "Encode VALUE to bencode format as a byte vector."
  (babel:string-to-octets (bencode-encode value) :encoding :utf-8))

(defun bencode-decode-from-octets (octets)
  "Decode bencode from a byte vector."
  (bencode-decode (babel:octets-to-string octets :encoding :utf-8)))
