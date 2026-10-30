(uiop:define-package :lem/mpd
  (:use :cl :lem)
  (:import-from :usocket
                :socket-error)
  (:import-from :lem/directory-mode/attributes
                :current-directory-attribute
                :directory-attribute
                :file-attribute)
  (:import-from :lem/mpd/client
                :mpd-error
                :mpd-connection-error
                :mpd-connection-host
                :mpd-connection-port
                :connect
                :disconnect
                :connection-open-p
                :response-value
                :get-status
                :get-current-song
                :list-mpd-directory
                :list-queue
                :list-playlists
                :list-playlist-songs
                :list-outputs
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
                :save-queue)
  (:export :mpd
           :mpd-connect
           :mpd-mode
           :*mpd-mode-keymap*
           :*mpd-host*
           :*mpd-port*
           :*mpd-password*))
(in-package :lem/mpd)

(defvar *mpd-host* (or (uiop:getenv "MPD_HOST") "localhost")
  "Host name of the MPD server.

MPD_HOST may contain a password in the form PASSWORD@HOST, matching the
convention used by other MPD clients.")

(defvar *mpd-port* (let ((port (uiop:getenv "MPD_PORT")))
                     (or (and port (ignore-errors (parse-integer port)))
                         6600))
  "TCP port of the MPD server.")

(defvar *mpd-password* nil
  "Password for the MPD server, or NIL when no password is required.")

(defvar *mpd-connection* nil
  "The current connection to the MPD server.")

(defvar *mpd-buffer-name* "*MPD*")

(defvar *mpd-mode-keymap* (make-keymap :description '*mpd-mode-keymap*))

(define-key *global-keymap* "C-c m" 'mpd)

(define-keys *mpd-mode-keymap*
  ("Return" 'mpd-open)
  ("n" 'next-line)
  ("p" 'previous-line)
  ("C-n" 'next-line)
  ("C-p" 'previous-line)
  ("^" 'mpd-up)
  ("Backspace" 'mpd-up)
  ("g" 'mpd-refresh)
  ("r" 'mpd-refresh)
  ("q" 'quit-active-window)
  ("M-q" 'quit-active-window)
  ("a" 'mpd-add-to-queue)
  ("Space" 'mpd-toggle-pause)
  ("s" 'mpd-stop)
  ("<" 'mpd-previous)
  (">" 'mpd-next)
  ("+" 'mpd-volume-up)
  ("-" 'mpd-volume-down)
  ("d" 'mpd-delete)
  ("C" 'mpd-clear-queue)
  ("S" 'mpd-save-queue)
  ("?" 'mpd-help))

(define-attribute mpd-playing-attribute
  (t :foreground :base0B :bold t))

(define-attribute mpd-special-attribute
  (t :foreground :base0E :bold t))

(define-attribute mpd-dim-attribute
  (t :foreground :base03))

(define-major-mode mpd-mode ()
    (:name "MPD"
     :keymap *mpd-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(defstruct (mpd-entry (:constructor make-mpd-entry
                         (&key type name uri metadata position)))
  type
  name
  uri
  metadata
  position)

(defun split-host-and-password (host password)
  (if password
      (values host password)
      (let ((at (position #\@ host)))
        (if at
            (values (subseq host (1+ at)) (subseq host 0 at))
            (values host nil)))))

(defun current-connection ()
  (unless (and *mpd-connection* (connection-open-p *mpd-connection*))
    (multiple-value-bind (host password)
        (split-host-and-password *mpd-host* *mpd-password*)
      (setf *mpd-connection* (connect host *mpd-port* :password password))))
  *mpd-connection*)

(defmacro with-mpd-connection ((connection) &body body)
  "Run BODY with a live MPD CONNECTION, reporting failures as editor errors."
  `(handler-case
       (let ((,connection (current-connection)))
         ,@body)
     (mpd-connection-error (condition)
       (setf *mpd-connection* nil)
       (editor-error "~A" condition))
     (mpd-error (condition)
       (editor-error "~A" condition))
     (socket-error (condition)
       (setf *mpd-connection* nil)
       (editor-error "MPD connection failed: ~A" condition))
     (stream-error (condition)
       (setf *mpd-connection* nil)
       (editor-error "MPD connection lost: ~A" condition))))

(defun uri-basename (uri)
  (let ((slash (position #\/ uri :from-end t)))
    (subseq uri (if slash (1+ slash) 0))))

(defun parent-uri (uri)
  (let ((slash (position #\/ uri :from-end t)))
    (and slash (subseq uri 0 slash))))

(defun song-label (metadata)
  (let ((title (response-value metadata "title"))
        (artist (response-value metadata "artist"))
        (file (response-value metadata "file")))
    (cond ((and artist title)
           (format nil "~A - ~A" artist title))
          (title
           title)
          (artist
           (format nil "~A - ~A" artist (uri-basename file)))
          (t
           (uri-basename (or file ""))))))

(defun format-duration (seconds)
  (multiple-value-bind (minutes remaining-seconds)
      (floor seconds 60)
    (format nil "~D:~2,'0D" minutes remaining-seconds)))

(defun song-duration (metadata)
  (let ((duration (or (response-value metadata "duration")
                      (response-value metadata "time"))))
    (when duration
      (let ((seconds (parse-integer duration :junk-allowed t)))
        (when seconds
          (format-duration seconds))))))

(defun song-duration-suffix (metadata)
  (let ((duration (song-duration metadata)))
    (when duration
      (format nil "  (~A)" duration))))

(defun output-enabled-p (entry)
  (string= "1" (response-value (mpd-entry-metadata entry) "outputenabled" "")))

(defun entry-label (entry)
  (case (mpd-entry-type entry)
    (:parent
     "..")
    (:directory
     (format nil "~A/" (mpd-entry-name entry)))
    (:output
     (format nil "[~A] ~A"
             (if (output-enabled-p entry) "x" " ")
             (mpd-entry-name entry)))
    (:file
     (let ((metadata (mpd-entry-metadata entry)))
       (if metadata (song-label metadata) (mpd-entry-name entry))))
    ((:queue-item :playlist-item)
     (song-label (mpd-entry-metadata entry)))
    (t
     (or (mpd-entry-name entry) ""))))

(defun entry-attribute (entry)
  (case (mpd-entry-type entry)
    ((:parent :directory :playlist)
     'directory-attribute)
    ((:queue-root :playlists-root :outputs-root :output)
     'mpd-special-attribute)
    (t
     'file-attribute)))

(defun insert-entry-line (point entry &key marker prefix suffix)
  (with-point ((start point))
    (insert-string point "  ")
    (insert-string point (or marker "  ")
                   :attribute 'mpd-playing-attribute)
    (when prefix
      (insert-string point prefix :attribute 'mpd-dim-attribute))
    (insert-string point (entry-label entry)
                   :attribute (entry-attribute entry))
    (when suffix
      (insert-string point suffix :attribute 'mpd-dim-attribute))
    (insert-character point #\newline)
    (put-text-property start point 'mpd-entry entry)
    (put-text-property start point 'read-only t)))

(defun insert-message-line (point text)
  (insert-string point (concatenate 'string "  " text)
                 :attribute 'mpd-dim-attribute)
  (insert-character point #\newline))

(defun current-entry ()
  (with-point ((point (current-point)))
    (line-start point)
    (text-property-at point 'mpd-entry)))

(defun records-to-entries (records)
  (loop :for record :in records
        :for directory := (response-value record "directory")
        :for file := (response-value record "file")
        :when directory
          :collect (make-mpd-entry :type :directory
                                   :name (uri-basename directory)
                                   :uri directory
                                   :metadata record)
        :when file
          :collect (make-mpd-entry :type :file
                                   :name (uri-basename file)
                                   :uri file
                                   :metadata record)))

(defun sort-lsinfo-entries (entries)
  (stable-sort entries
               (lambda (a b)
                 (let ((rank-a (if (eq (mpd-entry-type a) :directory) 0 1))
                       (rank-b (if (eq (mpd-entry-type b) :directory) 0 1)))
                   (if (= rank-a rank-b)
                       (string-lessp (mpd-entry-name a) (mpd-entry-name b))
                       (< rank-a rank-b))))))

(defun view-title (buffer connection)
  (let ((base (format nil "mpd://~A:~D"
                      (mpd-connection-host connection)
                      (mpd-connection-port connection))))
    (case (buffer-value buffer 'mpd-view :root)
      (:root (format nil "~A/" base))
      (:browse (format nil "~A/~A/" base (buffer-value buffer 'mpd-path)))
      (:queue (format nil "~A/[queue]" base))
      (:playlists (format nil "~A/[playlists]" base))
      (:playlist (format nil "~A/[playlists]/~A"
                         base
                         (buffer-value buffer 'mpd-playlist-name)))
      (:outputs (format nil "~A/[outputs]" base)))))

(defun insert-header (point buffer connection status current)
  (insert-string point (view-title buffer connection)
                 :attribute 'current-directory-attribute)
  (let ((state (response-value status "state")))
    (when state
      (insert-string point (format nil "  [~A]" state)
                     :attribute 'mpd-dim-attribute)))
  (let ((volume (response-value status "volume")))
    (when (and volume (not (string= volume "-1")))
      (insert-string point (format nil "  Vol ~A%" volume)
                     :attribute 'mpd-dim-attribute)))
  (when current
    (insert-string point (format nil "  ~A" (song-label current))
                   :attribute 'mpd-playing-attribute))
  (insert-string point (format nil "~%~%")))

(defun render-root (point connection status current-uri)
  (let* ((queue-length (or (response-value status "playlistlength") "0"))
         (playlists (list-playlists connection))
         (records (remove-if (lambda (record)
                               (response-value record "playlist"))
                             (list-mpd-directory connection)))
         (entries (sort-lsinfo-entries (records-to-entries records))))
    (insert-entry-line point
                       (make-mpd-entry :type :queue-root
                                       :name (format nil "[Queue] (~A)" queue-length)))
    (insert-entry-line point
                       (make-mpd-entry :type :playlists-root
                                       :name (format nil "[Playlists] (~D)" (length playlists))))
    (insert-entry-line point
                       (make-mpd-entry :type :outputs-root :name "[Outputs]"))
    (dolist (entry entries)
      (insert-entry-line point entry
                         :marker (when (and current-uri
                                            (eq (mpd-entry-type entry) :file)
                                            (string= (mpd-entry-uri entry) current-uri))
                                   "▶ ")))))

(defun render-directory (point connection path current-uri)
  (let* ((records (list-mpd-directory connection path))
         (entries (sort-lsinfo-entries (records-to-entries records))))
    (insert-entry-line point
                       (make-mpd-entry :type :parent
                                       :name ".."
                                       :uri (parent-uri path)))
    (dolist (entry entries)
      (insert-entry-line point entry
                         :marker (when (and current-uri
                                            (eq (mpd-entry-type entry) :file)
                                            (string= (mpd-entry-uri entry) current-uri))
                                   "▶ ")))
    (when (null entries)
      (insert-message-line point "(empty directory)"))))

(defun render-queue (point connection status)
  (let* ((songs (list-queue connection))
         (position (let ((value (response-value status "song")))
                     (and value (parse-integer value :junk-allowed t)))))
    (if (null songs)
        (insert-message-line point "(queue is empty)")
        (loop :for song :in songs
              :for index :from 0
              :do (insert-entry-line point
                                     (make-mpd-entry :type :queue-item
                                                     :metadata song
                                                     :position index)
                                     :marker (when (eql index position) "▶ ")
                                     :prefix (format nil "~D. " (1+ index))
                                     :suffix (song-duration-suffix song))))))

(defun render-playlists (point connection)
  (let ((playlists (list-playlists connection)))
    (if (null playlists)
        (insert-message-line point "(no stored playlists)")
        (dolist (record playlists)
          (insert-entry-line point
                             (make-mpd-entry :type :playlist
                                             :name (response-value record "playlist")
                                             :metadata record)
                             :suffix (let ((modified (response-value record "last-modified")))
                                       (when modified
                                         (format nil "  ~A" modified))))))))

(defun render-playlist (point connection name current-uri)
  (if (null name)
      (insert-message-line point "(no playlist selected)")
      (let ((songs (list-playlist-songs connection name)))
        (if (null songs)
            (insert-message-line point "(playlist is empty)")
            (loop :for song :in songs
                  :for index :from 0
                  :do (insert-entry-line point
                                         (make-mpd-entry :type :playlist-item
                                                         :metadata song
                                                         :position index)
                                         :marker (when (and current-uri
                                                            (string= (response-value song "file")
                                                                     current-uri))
                                                   "▶ ")
                                         :prefix (format nil "~D. " (1+ index))
                                         :suffix (song-duration-suffix song)))))))

(defun render-outputs (point connection)
  (let ((outputs (list-outputs connection)))
    (if (null outputs)
        (insert-message-line point "(no outputs)")
        (dolist (record outputs)
          (insert-entry-line point
                             (make-mpd-entry :type :output
                                             :name (response-value record "outputname")
                                             :metadata record))))))

(defun render-buffer (buffer &key line)
  "Redraw BUFFER from the MPD server."
  (with-mpd-connection (connection)
    (let* ((view (buffer-value buffer 'mpd-view :root))
           (status (get-status connection))
           (current (get-current-song connection))
           (current-uri (and current (response-value current "file"))))
      (with-buffer-read-only buffer nil
        (let ((*inhibit-read-only* t)
              (point (buffer-point buffer))
              (old-line (line-number-at-point (buffer-point buffer))))
          (erase-buffer buffer)
          (buffer-start point)
          (insert-header point buffer connection status current)
          (case view
            (:root
             (render-root point connection status current-uri))
            (:browse
             (render-directory point connection
                               (buffer-value buffer 'mpd-path)
                               current-uri))
            (:queue
             (render-queue point connection status))
            (:playlists
             (render-playlists point connection))
            (:playlist
             (render-playlist point connection
                              (buffer-value buffer 'mpd-playlist-name)
                              current-uri))
            (:outputs
             (render-outputs point connection)))
          (let ((last-line (line-number-at-point (buffer-end point))))
            (move-to-line point (max 1 (min (or line old-line 3) last-line)))))))))

(defun show-root (buffer)
  (setf (buffer-value buffer 'mpd-view) :root
        (buffer-value buffer 'mpd-path) nil)
  (render-buffer buffer :line 3))

(defun show-directory (buffer uri)
  (if uri
      (setf (buffer-value buffer 'mpd-view) :browse
            (buffer-value buffer 'mpd-path) uri)
      (setf (buffer-value buffer 'mpd-view) :root
            (buffer-value buffer 'mpd-path) nil))
  (render-buffer buffer :line 3))

(defun show-view (buffer view)
  (setf (buffer-value buffer 'mpd-view) view)
  (render-buffer buffer :line 3))

(defun play-current-entry (entry)
  (with-mpd-connection (connection)
    (play-uri connection (mpd-entry-uri entry)))
  (message "Playing ~A" (entry-label entry))
  (render-buffer (current-buffer)))

(defun play-queue-item (entry)
  (with-mpd-connection (connection)
    (play-queue-index connection (mpd-entry-position entry)))
  (message "Playing queue item ~D" (1+ (mpd-entry-position entry)))
  (render-buffer (current-buffer)))

(defun play-playlist-item (entry)
  (let ((name (buffer-value (current-buffer) 'mpd-playlist-name)))
    (with-mpd-connection (connection)
      (play-playlist-index connection name (mpd-entry-position entry)))
    (setf (buffer-value (current-buffer) 'mpd-view) :queue)
    (render-buffer (current-buffer) :line 3)))

(defun toggle-output-entry (entry)
  (let ((output-id (response-value (mpd-entry-metadata entry) "outputid")))
    (with-mpd-connection (connection)
      (toggle-output connection output-id))
    (message "Toggled output ~A" (mpd-entry-name entry))
    (render-buffer (current-buffer))))

(define-command mpd-open () ()
  "Open the directory at point, or play the song at point.

Inside the queue view, play the queue item at point.  Inside the
playlists view, open the playlist at point.  Inside the outputs view,
toggle the output at point."
  (let* ((buffer (current-buffer))
         (entry (current-entry)))
    (unless entry
      (editor-error "No MPD entry at point."))
    (case (mpd-entry-type entry)
      (:parent
       (show-directory buffer (mpd-entry-uri entry)))
      (:directory
       (show-directory buffer (mpd-entry-uri entry)))
      (:file
       (play-current-entry entry))
      (:queue-root
       (show-view buffer :queue))
      (:playlists-root
       (show-view buffer :playlists))
      (:outputs-root
       (show-view buffer :outputs))
      (:playlist
       (setf (buffer-value buffer 'mpd-view) :playlist
             (buffer-value buffer 'mpd-playlist-name) (mpd-entry-name entry))
       (render-buffer buffer :line 3))
      (:queue-item
       (play-queue-item entry))
      (:playlist-item
       (play-playlist-item entry))
      (:output
       (toggle-output-entry entry)))))

(define-command mpd-up () ()
  "Go to the parent directory or view."
  (let* ((buffer (current-buffer))
         (view (buffer-value buffer 'mpd-view :root)))
    (case view
      (:browse
       (show-directory buffer (parent-uri (buffer-value buffer 'mpd-path))))
      ((:queue :playlists :outputs)
       (show-root buffer))
      (:playlist
       (setf (buffer-value buffer 'mpd-view) :playlists)
       (render-buffer buffer :line 3))
      (t
       (message "Already at the top.")))))

(define-command mpd-refresh () ()
  "Refresh the MPD buffer from the server."
  (render-buffer (current-buffer)))

(define-command mpd-add-to-queue () ()
  "Add the song, directory or playlist at point to the MPD queue."
  (let ((entry (current-entry)))
    (unless entry
      (editor-error "No MPD entry at point."))
    (case (mpd-entry-type entry)
      ((:file :directory)
       (with-mpd-connection (connection)
         (add-uri connection (mpd-entry-uri entry))))
      (:playlist
       (with-mpd-connection (connection)
         (load-playlist connection (mpd-entry-name entry))))
      (:playlist-item
       (with-mpd-connection (connection)
         (add-uri connection
                  (response-value (mpd-entry-metadata entry) "file"))))
      (t
       (editor-error "Cannot add this entry to the queue.")))
    (message "Added ~A to the queue" (entry-label entry))))

(define-command mpd-toggle-pause () ()
  "Play, pause or resume MPD playback."
  (with-mpd-connection (connection)
    (toggle-pause connection))
  (render-buffer (current-buffer)))

(define-command mpd-stop () ()
  "Stop MPD playback."
  (with-mpd-connection (connection)
    (stop-playback connection))
  (render-buffer (current-buffer)))

(define-command mpd-next () ()
  "Skip to the next song in the MPD queue."
  (with-mpd-connection (connection)
    (next-song connection))
  (render-buffer (current-buffer)))

(define-command mpd-previous () ()
  "Go back to the previous song in the MPD queue."
  (with-mpd-connection (connection)
    (previous-song connection))
  (render-buffer (current-buffer)))

(define-command mpd-volume-up () ()
  "Increase the MPD volume by five percent."
  (with-mpd-connection (connection)
    (adjust-volume connection 5))
  (message "Volume +5")
  (render-buffer (current-buffer)))

(define-command mpd-volume-down () ()
  "Decrease the MPD volume by five percent."
  (with-mpd-connection (connection)
    (adjust-volume connection -5))
  (message "Volume -5")
  (render-buffer (current-buffer)))

(define-command mpd-delete () ()
  "Delete the queue item, stored playlist or playlist song at point."
  (let* ((buffer (current-buffer))
         (view (buffer-value buffer 'mpd-view :root))
         (entry (current-entry)))
    (unless entry
      (editor-error "No MPD entry at point."))
    (case view
      (:queue
       (unless (eq (mpd-entry-type entry) :queue-item)
         (editor-error "No queue item at point."))
       (with-mpd-connection (connection)
         (delete-queue-index connection (mpd-entry-position entry)))
       (message "Deleted queue item.")
       (render-buffer buffer))
      (:playlists
       (unless (eq (mpd-entry-type entry) :playlist)
         (editor-error "No playlist at point."))
       (when (prompt-for-y-or-n-p
              (format nil "Delete playlist ~A? " (mpd-entry-name entry)))
         (with-mpd-connection (connection)
           (delete-playlist connection (mpd-entry-name entry)))
         (message "Deleted playlist ~A" (mpd-entry-name entry))
         (render-buffer buffer)))
      (:playlist
       (unless (eq (mpd-entry-type entry) :playlist-item)
         (editor-error "No playlist song at point."))
       (when (prompt-for-y-or-n-p "Delete this song from the playlist? ")
         (with-mpd-connection (connection)
           (delete-playlist-song connection
                                 (buffer-value buffer 'mpd-playlist-name)
                                 (mpd-entry-position entry)))
         (message "Deleted playlist song.")
         (render-buffer buffer)))
      (t
       (editor-error "Nothing to delete in this view.")))))

(define-command mpd-clear-queue () ()
  "Clear the MPD queue after confirmation."
  (when (prompt-for-y-or-n-p "Clear the MPD queue? ")
    (with-mpd-connection (connection)
      (clear-queue connection))
    (message "Cleared the queue.")
    (render-buffer (current-buffer))))

(define-command mpd-save-queue (name) ((:string "Playlist name: "))
  "Save the current MPD queue as the stored playlist NAME."
  (with-mpd-connection (connection)
    (save-queue connection name))
  (message "Saved the queue as ~A" name)
  (render-buffer (current-buffer)))

(define-command mpd-help () ()
  "Show the MPD browser keybindings."
  (with-pop-up-typeout-window (stream (make-buffer "*MPD help*") :erase t)
    (format stream "MPD browser~2%
Return  open directory, play song, or open playlist
^       go to parent directory or view
n / p   move to next / previous line
g       refresh
a       add entry at point to the queue
Space   play / pause
s       stop
< / >   previous / next song
+ / -   volume up / down
d       delete queue item or stored playlist entry
C       clear the queue
S       save the queue as a stored playlist
q       close this window
?       show this help~%")))

(defun make-mpd-buffer ()
  (let ((buffer (make-buffer *mpd-buffer-name*
                             :enable-undo-p nil
                             :read-only-p t)))
    (setf (buffer-value buffer 'mpd-view) :root)
    (change-buffer-mode buffer 'mpd-mode)
    buffer))

(define-command mpd () ()
  "Open the MPD client buffer."
  (let ((buffer (or (get-buffer *mpd-buffer-name*)
                    (make-mpd-buffer))))
    (switch-to-buffer buffer)
    (render-buffer buffer)))

(define-command mpd-connect (host port) ((:string "MPD host: ") (:string "Port: "))
  "Connect to the MPD server at HOST:PORT and open the MPD client buffer."
  (let ((port-number (ignore-errors (parse-integer port))))
    (unless (and port-number (<= 1 port-number 65535))
      (editor-error "Invalid MPD port: ~A" port))
    (when *mpd-connection*
      (disconnect *mpd-connection*)
      (setf *mpd-connection* nil))
    (setf *mpd-host* (if (string= host "") "localhost" host)
          *mpd-port* port-number)
    (mpd)))
