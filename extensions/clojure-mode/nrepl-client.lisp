(defpackage :lem-clojure-mode/nrepl-client
  (:use :cl :lem)
  (:import-from :lem-clojure-mode/bencode
                :bencode-encode
                :bencode-decode
                :bencode-decode-from-stream)
  (:export :*nrepl-connection*
           :nrepl-connection
           :nrepl-connection-p
           :nrepl-connection-host
           :nrepl-connection-port
           :nrepl-connection-session
           :nrepl-connect
           :nrepl-disconnect
           :nrepl-connected-p
           :nrepl-send
           :nrepl-send-sync
           :nrepl-eval
           :nrepl-eval-sync
           :nrepl-interrupt
           :nrepl-clone-session
           :nrepl-close-session
           :nrepl-describe
           :nrepl-completions
           :check-nrepl-connection
           ;; Message utilities
           :make-nrepl-message
           :generate-message-id
           ;; Response utilities
           :nrepl-response-value
           :nrepl-response-out
           :nrepl-response-err
           :nrepl-response-exception))

(in-package :lem-clojure-mode/nrepl-client)

;;;; nREPL Connection Structure

(defvar *nrepl-connection* nil
  "The current nREPL connection.")

(defvar *message-id-counter* 0
  "Counter for generating unique message IDs.")

(defstruct nrepl-connection
  "Represents a connection to an nREPL server."
  host
  port
  socket
  stream
  session
  (pending-responses (make-hash-table :test 'equal))
  (response-handlers (make-hash-table :test 'equal))
  reader-thread)

;;;; Connection Management

(defun generate-message-id ()
  "Generate a unique message ID."
  (format nil "msg-~D-~D" (get-universal-time) (incf *message-id-counter*)))

(defun nrepl-connect (host port)
  "Connect to an nREPL server at HOST:PORT."
  (when *nrepl-connection*
    (nrepl-disconnect))
  (handler-case
      (let* ((socket (usocket:socket-connect host port
                                             :element-type '(unsigned-byte 8)))
             (stream (usocket:socket-stream socket))
             (connection (make-nrepl-connection
                          :host host
                          :port port
                          :socket socket
                          :stream stream)))
        ;; Clone a session
        (let ((session-id (nrepl-clone-session-sync connection)))
          (setf (nrepl-connection-session connection) session-id))
        ;; Start reader thread
        (setf (nrepl-connection-reader-thread connection)
              (bt:make-thread
               (lambda () (nrepl-reader-loop connection))
               :name "nREPL Reader"))
        (setf *nrepl-connection* connection)
        (message "Connected to nREPL server at ~A:~D" host port)
        connection)
    (usocket:socket-error (e)
      (editor-error "Failed to connect to nREPL server: ~A" e))))

(defun nrepl-disconnect ()
  "Disconnect from the current nREPL server."
  (when *nrepl-connection*
    (let ((connection *nrepl-connection*))
      ;; Close session
      (ignore-errors
        (when (nrepl-connection-session connection)
          (nrepl-close-session connection)))
      ;; Stop reader thread
      (when (and (nrepl-connection-reader-thread connection)
                 (bt:thread-alive-p (nrepl-connection-reader-thread connection)))
        (bt:destroy-thread (nrepl-connection-reader-thread connection)))
      ;; Close socket
      (ignore-errors
        (usocket:socket-close (nrepl-connection-socket connection)))
      (setf *nrepl-connection* nil)
      (message "Disconnected from nREPL server"))))

(defun nrepl-connected-p ()
  "Check if currently connected to an nREPL server."
  (and *nrepl-connection*
       (nrepl-connection-socket *nrepl-connection*)
       (usocket:socket-stream (nrepl-connection-socket *nrepl-connection*))))

(defun check-nrepl-connection ()
  "Signal an error if not connected to nREPL."
  (unless (nrepl-connected-p)
    (editor-error "Not connected to nREPL server. Use M-x clojure-connect")))

;;;; Message Sending

(defun make-nrepl-message (&rest pairs)
  "Create an nREPL message as a hash-table from key-value pairs."
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on pairs :by #'cddr
          :when value
          :do (setf (gethash (if (keywordp key)
                                 (string-downcase (symbol-name key))
                                 key)
                             table)
                    value))
    table))

(defun nrepl-send (connection message &optional callback)
  "Send MESSAGE to the nREPL server and optionally register CALLBACK."
  (let* ((id (or (gethash "id" message) (generate-message-id)))
         (session (nrepl-connection-session connection)))
    (setf (gethash "id" message) id)
    (when (and session (not (gethash "session" message)))
      (setf (gethash "session" message) session))
    (when callback
      (setf (gethash id (nrepl-connection-response-handlers connection))
            callback))
    (let ((encoded (bencode-encode message))
          (stream (nrepl-connection-stream connection)))
      (write-sequence (babel:string-to-octets encoded :encoding :utf-8) stream)
      (force-output stream))
    id))

(defun nrepl-send-sync (connection message &key (timeout 30))
  "Send MESSAGE synchronously and wait for response."
  (let* ((id (generate-message-id))
         (responses nil)
         (done nil)
         (lock (bt:make-lock))
         (cv (bt:make-condition-variable)))
    (setf (gethash "id" message) id)
    (setf (gethash id (nrepl-connection-response-handlers connection))
          (lambda (response)
            (bt:with-lock-held (lock)
              (push response responses)
              (when (member "done" (gethash "status" response) :test #'equal)
                (setf done t)
                (bt:condition-notify cv)))))
    (nrepl-send connection message)
    (bt:with-lock-held (lock)
      (loop :until done
            :do (bt:condition-wait cv lock :timeout timeout)))
    (nreverse responses)))

;;;; Reader Loop

(defun nrepl-reader-loop (connection)
  "Read responses from the nREPL server."
  (handler-case
      (let ((stream (nrepl-connection-stream connection)))
        (loop
          (let* ((response (read-bencode-message stream))
                 (id (gethash "id" response))
                 (handler (gethash id (nrepl-connection-response-handlers connection))))
            (when handler
              (send-event (lambda () (funcall handler response))))
            ;; Clean up handler if done
            (when (member "done" (gethash "status" response) :test #'equal)
              (remhash id (nrepl-connection-response-handlers connection))))))
    (end-of-file ()
      (send-event (lambda () (message "nREPL connection closed"))))
    (error (e)
      (send-event (lambda () (message "nREPL reader error: ~A" e))))))

(defun read-bencode-message (stream)
  "Read a complete bencode message from STREAM."
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
    ;; Read bytes until we have a complete message
    (loop
      (let ((byte (read-byte stream)))
        (vector-push-extend byte buffer)
        (handler-case
            (let ((string (babel:octets-to-string buffer :encoding :utf-8)))
              (return (bencode-decode string)))
          (lem-clojure-mode/bencode:bencode-error ()
            ;; Not complete yet, continue reading
            nil))))))

;;;; nREPL Operations

(defun nrepl-clone-session-sync (connection)
  "Clone a new session and return the session ID."
  (let* ((message (make-nrepl-message :op "clone"))
         (responses (nrepl-send-sync connection message)))
    (when responses
      (gethash "new-session" (first responses)))))

(defun nrepl-clone-session (connection callback)
  "Clone a new session asynchronously."
  (let ((message (make-nrepl-message :op "clone")))
    (nrepl-send connection message
                (lambda (response)
                  (funcall callback (gethash "new-session" response))))))

(defun nrepl-close-session (connection)
  "Close the current session."
  (when (nrepl-connection-session connection)
    (let ((message (make-nrepl-message
                    :op "close"
                    :session (nrepl-connection-session connection))))
      (nrepl-send connection message))))

(defun nrepl-describe (connection callback)
  "Describe the nREPL server capabilities."
  (let ((message (make-nrepl-message :op "describe")))
    (nrepl-send connection message callback)))

(defun nrepl-eval (code &key ns callback file line column)
  "Evaluate CODE in the nREPL server."
  (check-nrepl-connection)
  (let ((message (make-nrepl-message
                  :op "eval"
                  :code code
                  :ns ns
                  :file file
                  :line line
                  :column column)))
    (nrepl-send *nrepl-connection* message callback)))

(defun nrepl-eval-sync (code &key ns (timeout 30))
  "Evaluate CODE synchronously and return all responses."
  (check-nrepl-connection)
  (let ((message (make-nrepl-message
                  :op "eval"
                  :code code
                  :ns ns)))
    (nrepl-send-sync *nrepl-connection* message :timeout timeout)))

(defun nrepl-interrupt (connection)
  "Interrupt the current evaluation."
  (let ((message (make-nrepl-message
                  :op "interrupt"
                  :session (nrepl-connection-session connection))))
    (nrepl-send connection message)))

(defun nrepl-completions (prefix ns callback)
  "Get completions for PREFIX in namespace NS."
  (check-nrepl-connection)
  (let ((message (make-nrepl-message
                  :op "completions"
                  :prefix prefix
                  :ns ns)))
    (nrepl-send *nrepl-connection* message callback)))

;;;; Response Processing Utilities

(defun nrepl-response-value (responses)
  "Extract the value from nREPL responses."
  (loop :for response :in responses
        :for value := (gethash "value" response)
        :when value :return value))

(defun nrepl-response-out (responses)
  "Extract stdout from nREPL responses."
  (with-output-to-string (out)
    (loop :for response :in responses
          :for text := (gethash "out" response)
          :when text :do (write-string text out))))

(defun nrepl-response-err (responses)
  "Extract stderr from nREPL responses."
  (with-output-to-string (out)
    (loop :for response :in responses
          :for text := (gethash "err" response)
          :when text :do (write-string text out))))

(defun nrepl-response-exception (responses)
  "Extract exception info from nREPL responses."
  (loop :for response :in responses
        :for ex := (gethash "ex" response)
        :when ex :return ex))
