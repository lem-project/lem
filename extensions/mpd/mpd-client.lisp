(uiop:define-package :lem/mpd/client
  (:use :cl)
  (:import-from :usocket
                :socket-connect
                :socket-close
                :socket-stream
                :socket-error)
  (:export
   ;; Conditions
   :mpd-error
   :mpd-error-message
   :mpd-connection-error
   ;; Connections
   :mpd-connection
   :mpd-connection-host
   :mpd-connection-port
   :connect
   :disconnect
   :connection-open-p
   ;; Protocol
   :send-command
   :send-command-list
   :response-value
   :response-values
   :parse-records
   ;; Queries
   :get-status
   :get-current-song
   :list-mpd-directory
   :list-queue
   :list-playlists
   :list-playlist-songs
   :list-outputs
   ;; Queue and playback
   :add-uri
   :play-uri
   :play-queue-index
   :play-playlist-index
   :delete-queue-index
   :clear-queue
   :toggle-pause
   :stop-playback
   :next-song
   :previous-song
   :adjust-volume
   :toggle-output
   :load-playlist
   :delete-playlist
   :delete-playlist-song
   :save-queue))
(in-package :lem/mpd/client)

(define-condition mpd-error (error)
  ((message
    :initarg :message
    :reader mpd-error-message))
  (:report (lambda (condition stream)
             (format stream "MPD: ~A" (mpd-error-message condition)))))

(define-condition mpd-connection-error (mpd-error)
  ())

(defstruct (mpd-connection (:constructor make-mpd-connection (&key host port socket stream)))
  host
  port
  socket
  stream)

(defun decode-line (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun read-line-octets (stream)
  "Read one newline-terminated line from STREAM as a UTF-8 string.
Returns NIL at end of file."
  (let ((octets (make-array 128
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (cond ((null byte)
               (return (when (plusp (fill-pointer octets))
                         (decode-line octets))))
              ((= byte 10)
               (return (decode-line octets)))
              ((= byte 13))
              (t
               (vector-push-extend byte octets)))))))

(defun write-protocol-line (connection line)
  (let ((stream (mpd-connection-stream connection)))
    (write-sequence (babel:string-to-octets (concatenate 'string line (string #\newline))
                                            :encoding :utf-8)
                    stream)
    (finish-output stream)))

(defun encode-argument (argument)
  "Escape ARGUMENT as a quoted MPD command argument."
  (check-type argument string)
  (when (find #\Newline argument)
    (error 'mpd-error :message "MPD command arguments cannot contain newlines"))
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop :for char :across argument
          :do (case char
                (#\\ (write-string "\\\\" stream))
                (#\" (write-string "\\\"" stream))
                (t (write-char char stream))))
    (write-char #\" stream)))

(defun format-command (command arguments)
  (format nil "~A~{ ~A~}" command (mapcar #'encode-argument arguments)))

(defun parse-response-line (line)
  "Split LINE into a (KEY . VALUE) pair, downcasing KEY.
Return NIL when LINE is not a key/value line."
  (let ((colon (position #\: line)))
    (when colon
      (let ((value-start (1+ colon)))
        (cons (string-downcase (subseq line 0 colon))
              (if (and (< value-start (length line))
                       (char= (char line value-start) #\Space))
                  (subseq line (1+ value-start))
                  (subseq line value-start)))))))

(defun read-response (connection)
  "Read a complete MPD response as an alist of (KEY . VALUE) strings."
  (let ((response '()))
    (loop
      (let ((line (read-line-octets (mpd-connection-stream connection))))
        (cond ((null line)
               (disconnect connection)
               (error 'mpd-connection-error :message "Connection closed by MPD"))
              ((string= line "OK")
               (return (nreverse response)))
              ((uiop:string-prefix-p "ACK " line)
               (error 'mpd-error :message (subseq line 4)))
              ((string= line "list_OK"))
              (t
               (let ((pair (parse-response-line line)))
                 (when pair
                   (push pair response)))))))))

(defun connect (host port &key password)
  "Connect to the MPD server at HOST:PORT.
Send PASSWORD first when it is non-NIL.  Signal MPD-CONNECTION-ERROR on
transport failures."
  (check-type host string)
  (check-type port (integer 1 65535))
  (let* ((socket (handler-case
                     (socket-connect host port
                                     :element-type '(unsigned-byte 8)
                                     :nodelay t)
                   (socket-error (condition)
                     (error 'mpd-connection-error
                            :message (format nil "Cannot connect to ~A:~D: ~A"
                                             host port condition)))))
         (connection (make-mpd-connection
                      :host host
                      :port port
                      :socket socket
                      :stream (socket-stream socket))))
    (handler-case
        (let ((banner (read-line-octets (mpd-connection-stream connection))))
          (unless (and banner (uiop:string-prefix-p "OK MPD " banner))
            (error 'mpd-connection-error
                   :message (format nil "Unexpected MPD greeting: ~A" banner)))
          (when password
            (send-command connection "password" password))
          connection)
      (error (condition)
        (disconnect connection)
        (error condition)))))

(defun disconnect (connection)
  "Close CONNECTION, ignoring any errors raised while doing so."
  (when connection
    (ignore-errors (socket-close (mpd-connection-socket connection)))
    (setf (mpd-connection-socket connection) nil
          (mpd-connection-stream connection) nil))
  (values))

(defun connection-open-p (connection)
  (and connection
       (mpd-connection-stream connection)
       (open-stream-p (mpd-connection-stream connection))))

(defun send-command (connection command &rest arguments)
  "Send COMMAND with ARGUMENTS to CONNECTION and return the response alist."
  (write-protocol-line connection (format-command command arguments))
  (read-response connection))

(defun send-command-list (connection commands)
  "Send COMMANDS atomically as an MPD command list.
COMMANDS is a list of (COMMAND &rest ARGUMENTS) lists.  Return the
concatenated response alist."
  (write-protocol-line connection "command_list_begin")
  (dolist (command commands)
    (write-protocol-line connection
                         (format-command (first command) (rest command))))
  (write-protocol-line connection "command_list_end")
  (read-response connection))

(defun response-value (response key &optional default)
  "Return the first value for KEY in RESPONSE."
  (let ((pair (assoc key response :test #'string=)))
    (if pair (cdr pair) default)))

(defun response-values (response key)
  "Return every value for KEY in RESPONSE."
  (loop :for pair :in response
        :when (string= (car pair) key)
          :collect (cdr pair)))

(defun parse-records (response &optional (start-keys '("file")))
  "Split RESPONSE into records.
A new record starts at every line whose key is a member of START-KEYS."
  (let ((records '())
        (current nil))
    (dolist (pair response)
      (if (member (car pair) start-keys :test #'string=)
          (progn
            (when current
              (push (nreverse current) records))
            (setf current (list pair)))
          (when current
            (push pair current))))
    (when current
      (push (nreverse current) records))
    (nreverse records)))

(defun get-status (connection)
  "Return the MPD status as an alist."
  (send-command connection "status"))

(defun get-current-song (connection)
  "Return the currently playing song as an alist, or NIL."
  (send-command connection "currentsong"))

(defun list-mpd-directory (connection &optional uri)
  "List the MPD database directory URI.
Records are lists of (KEY . VALUE) pairs, each starting with FILE,
DIRECTORY or PLAYLIST."
  (parse-records (if uri
                     (send-command connection "lsinfo" uri)
                     (send-command connection "lsinfo"))
                 '("file" "directory" "playlist")))

(defun list-queue (connection)
  "Return the current queue as a list of song records."
  (parse-records (send-command connection "playlistinfo")))

(defun list-playlists (connection)
  "Return the stored playlists as records."
  (parse-records (send-command connection "listplaylists") '("playlist")))

(defun list-playlist-songs (connection name)
  "Return the songs in the stored playlist NAME."
  (parse-records (send-command connection "listplaylistinfo" name)))

(defun list-outputs (connection)
  "Return the MPD outputs as records."
  (parse-records (send-command connection "outputs") '("outputid")))

(defun queue-length (connection)
  (let ((value (response-value (get-status connection) "playlistlength")))
    (if value (parse-integer value) 0)))

(defun add-uri (connection uri)
  "Append URI to the queue."
  (send-command connection "add" uri)
  (values))

(defun play-uri (connection uri)
  "Append URI to the queue and immediately play the first appended song."
  (let ((position (queue-length connection)))
    (send-command-list connection
                       (list (list "add" uri)
                             (list "play" (princ-to-string position))))
    position))

(defun play-queue-index (connection position)
  "Play the song at POSITION in the current queue."
  (send-command connection "play" (princ-to-string position))
  (values))

(defun play-playlist-index (connection name position)
  "Replace the queue with the stored playlist NAME and play POSITION."
  (send-command-list connection
                     (list (list "clear")
                           (list "load" name)
                           (list "play" (princ-to-string position))))
  (values))

(defun delete-queue-index (connection position)
  "Delete the song at POSITION from the current queue."
  (send-command connection "delete" (princ-to-string position))
  (values))

(defun clear-queue (connection)
  "Clear the current queue."
  (send-command connection "clear")
  (values))

(defun toggle-pause (connection)
  "Play, pause or resume according to the current MPD state."
  (let ((state (response-value (get-status connection) "state")))
    (cond ((string= state "play")
           (send-command connection "pause" "1"))
          ((string= state "pause")
           (send-command connection "pause" "0"))
          (t
           (send-command connection "play")))
    (values)))

(defun stop-playback (connection)
  "Stop playback."
  (send-command connection "stop")
  (values))

(defun next-song (connection)
  "Skip to the next song in the queue."
  (send-command connection "next")
  (values))

(defun previous-song (connection)
  "Go back to the previous song in the queue."
  (send-command connection "previous")
  (values))

(defun adjust-volume (connection delta)
  "Increase the volume by DELTA percent, or decrease it when negative."
  (check-type delta integer)
  (send-command connection "volume"
                (format nil "~A~D"
                        (if (plusp delta) "+" "-")
                        (abs delta)))
  (values))

(defun toggle-output (connection output-id)
  "Enable or disable the output OUTPUT-ID."
  (let* ((outputs (list-outputs connection))
         (output (find output-id outputs
                       :key (lambda (record)
                              (response-value record "outputid"))
                       :test #'string=))
         (enabled (and output
                       (string= "1" (response-value output "outputenabled" "")))))
    (send-command connection
                  (if enabled "disableoutput" "enableoutput")
                  output-id)
    (values)))

(defun load-playlist (connection name)
  "Append the stored playlist NAME to the current queue."
  (send-command connection "load" name)
  (values))

(defun delete-playlist (connection name)
  "Delete the stored playlist NAME."
  (send-command connection "rm" name)
  (values))

(defun delete-playlist-song (connection name position)
  "Delete the song at POSITION from the stored playlist NAME."
  (send-command connection "playlistdelete" name (princ-to-string position))
  (values))

(defun save-queue (connection name)
  "Save the current queue as the stored playlist NAME."
  (send-command connection "save" name)
  (values))
