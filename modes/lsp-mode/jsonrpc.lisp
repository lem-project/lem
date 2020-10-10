(defpackage :lem-lsp-mode/jsonrpc
  (:use :cl
        :lem-lsp-mode/json
        :alexandria)
  (:import-from :usocket)
  (:import-from :babel)
  (:import-from :rove)
  (:export :connect
           :send-message
           :receive-message))
(in-package :lem-lsp-mode/jsonrpc)

(defclass message (object)
  ((jsonrpc :initarg :jsonrpc
            :initform "2.0")))

(defclass request-message (message)
  ((id :initarg :id :reader request-message-id)
   (method :initarg :method)
   (params? :initarg :params)))

(defun content-length (content)
  (length (babel:string-to-octets content)))

(defun newline ()
  (load-time-value (concatenate 'string (string #\Return) (string #\LineFeed))))

(defun write-message (request-message stream)
  (let ((content (to-json-string request-message)))
    (format stream "Content-Length: ~D" (content-length content))
    (write-string (newline) stream)
    (write-string (newline) stream)
    (write-string content stream)))

(defun write-message-to-string (request-message)
  (with-output-to-string (stream)
    (write-message request-message stream)))

(defclass connection ()
  ((host
    :initarg :host
    :reader connection-host)
   (port
    :initarg :port
    :reader connection-port)
   (socket
    :initarg :socket
    :reader connection-socket)))

(defun connection-stream (connection)
  (usocket:socket-stream (connection-socket connection)))

(defun connect (&key (host "127.0.0.1")
                     (port (required-argument :port)))
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (make-instance 'connection
                   :host host
                   :port port
                   :socket socket)))

(defun send-message (connection request-message)
  (let ((string (write-message-to-string request-message))
        (stream (connection-stream connection)))
    (write-sequence (babel:string-to-octets string) stream)
    (force-output stream)))

(defun read-message-string (stream)
  (flet ((newline-string ()
           (load-time-value (concatenate 'string (string #\Return) (string #\LineFeed))))
         (read-one-line ()
           (with-output-to-string (out)
             (loop :for prev := nil :then c
                   :and c := (read-byte stream)
                   :do (write-char (code-char c) out)
                       (when (and (eql prev #.(char-code #\Return))
                                  (eql c #.(char-code #\LineFeed)))
                         (return)))))
         (parse-header (line)
           (ppcre:register-groups-bind (length) ("Content-Length:\\s*(\\d+)" line)
             (when length
               (parse-integer length)))))
    (let ((header (read-one-line)))
      (unless (equal (newline-string) (read-one-line))
        (error "invalid format"))
      (let* ((content-length (parse-header header))
             (seq (make-array content-length :element-type '(unsigned-byte 8))))
        (read-sequence seq stream)
        (babel:octets-to-string seq)))))

(defun receive-message (connection)
  (st-json:read-json-from-string (read-message-string (connection-stream connection))))

(defvar *last-request-id* 0)

(defun make-request-message (method params)
  (make-instance 'request-message
                 :id (incf *last-request-id*)
                 :method method
                 :params params))

(defun request (connection request-message)
  (send-message connection request-message)
  (request-message-id request-message))
    

#|
(defclass test-params (object)
  ((a
    :initarg :a)
   (b?
    :initarg :b)
   (c
    :initarg :c)))

(defun json-match-p (jso alist)
  (equalp (st-json::jso-alist jso)
          alist))

(rove:deftest check-required-initarg
  (rove:testing "Missing parameters"
    (flet ((make ()
             (let ((conditions '()))
               (handler-bind ((missing-parameter
                                (lambda (c)
                                  (push c conditions)
                                  (continue c))))
                 (make-instance 'test-params))
               (nreverse conditions)))
           (equals (condition class-name slot-name)
             (and (eq (slot-value condition 'class-name) class-name)
                  (eq (slot-value condition 'slot-name) slot-name))))
      (let ((conditions (make)))
        (rove:ok (= 2 (length conditions)))
        (rove:ok (equals (first conditions) 'test-params 'a))
        (rove:ok (equals (second conditions) 'test-params 'c))))))

(rove:deftest to-json
  (let ((json
          (to-json (make-instance 'test-params
                                  :a "test"
                                  :b 100
                                  :c '(1 2)))))
    (rove:ok (typep json 'st-json:jso))
    (rove:ok (json-match-p json
                           '(("a" . "test") ("b" . 100) ("c" 1 2))))))

(rove:deftest to-json-string
  (rove:ok
   (string= (to-json-string
             (make-instance 'test-params
                            :a "test"
                            :b 100
                            :c '(1 2)))
            "{\"a\":\"test\",\"b\":100,\"c\":[1,2]}")))

(rove:deftest create-request
  (let ((requestor (make-instance 'jsonrpc-requestor)))
    (rove:testing "first"
      (rove:ok (json-match-p (to-json (create-request requestor "foo" nil))
                             '(("jsonrpc" . "2.0")
                               ("id" . 0)
                               ("method" . "foo")))))
    (rove:testing "increment"
      (rove:ok (json-match-p (to-json (create-request requestor "bar" nil))
                             '(("jsonrpc" . "2.0")
                               ("id" . 1)
                               ("method" . "bar")))))
    (rove:testing "specify params"
      (rove:ok
       (json-match-p (to-json (create-request requestor
                                              "bar"
                                              (make-instance 'test-params :a 1 :b 2 :c 3)))
                     (let ((params (st-json:jso "a" 1 "b" 2 "c" 3)))
                       `(("jsonrpc" . "2.0")
                         ("id" . 2)
                         ("method" . "bar")
                         ("params" . ,params))))))))

(rove:deftest write-request
  (rove:ok
   (string= (with-output-to-string (stream)
              (write-request
               (create-request (make-instance 'jsonrpc-requestor)
                               "test/write-request"
                               (make-instance 'test-params
                                              :a "foo"
                                              :c 100))
               stream))
            (concatenate 'string
                         "Content-Length:83"
                         (string #\Return)
                         (string #\LineFeed)
                         (string #\Return)
                         (string #\LineFeed)
                         (st-json:write-json-to-string
                          (st-json:jso "jsonrpc" "2.0"
                                       "id" 0
                                       "method" "test/write-request"
                                       "params" (st-json:jso "a" "foo"
                                                             "c" 100)))))))
|#
