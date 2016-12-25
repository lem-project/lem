(in-package :lem)

(export '(buffer-input-stream
          make-buffer-input-stream
          buffer-output-stream
          make-buffer-output-stream
          minibuffer-input-stream
          make-minibuffer-input-stream
          editor-io-stream
          make-editor-io-stream))

(defclass buffer-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((unread-char
    :initform nil
    :accessor buffer-input-stream-unread-char)
   (point
    :initarg :point
    :accessor buffer-stream-point)))

(defun make-buffer-input-stream (&optional (point (current-point)))
  (make-instance 'buffer-input-stream
                 :point (copy-point point :temporary)))

#+nil
(defmethod trivial-gray-streams::close ((stream buffer-input-stream) &key abort)
  (declare (ignore abort))
  (delete-point (buffer-stream-point stream))
  t)

(defmethod trivial-gray-streams:stream-read-char ((stream buffer-input-stream))
  (let ((character (buffer-input-stream-unread-char stream)))
    (prog1 (cond (character
                  (setf (buffer-input-stream-unread-char stream) nil)
                  character)
                 (t
                  (character-at (buffer-stream-point stream))))
      (character-offset (buffer-stream-point stream) 1))))

(defmethod trivial-gray-streams:stream-unread-char ((stream buffer-input-stream) character)
  (setf (buffer-input-stream-unread-char stream) character)
  (character-offset (buffer-stream-point stream) -1)
  nil)

#+nil
(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream buffer-input-stream))
  )

(defmethod trivial-gray-streams:stream-peek-char ((stream buffer-input-stream))
  (character-at (buffer-stream-point stream)))

(defmethod trivial-gray-streams:stream-listen ((stream buffer-input-stream))
  (end-buffer-p (buffer-stream-point stream)))

(defmethod trivial-gray-streams:stream-read-line ((stream buffer-input-stream))
  (let ((start (copy-point (buffer-stream-point stream) :temporary)))
    (let ((string (points-to-string
                   start
                   (line-end (buffer-stream-point stream)))))
      (values string
              (not (line-offset (buffer-stream-point stream) 1))))))

(defmethod trivial-gray-streams:stream-clear-input ((stream buffer-input-stream))
  nil)

(defclass buffer-output-stream (trivial-gray-streams:fundamental-output-stream)
  ((interactive-update-p
    :initarg :interactive-update-p
    :accessor buffer-output-stream-interactive-update-p)
   (point
    :initarg :point
    :accessor buffer-stream-point)))

(defun make-buffer-stream-instance (class-name point
				    &optional
				      interactive-update-p)
  (make-instance class-name
                 :point (copy-point point :left-inserting)
                 :interactive-update-p interactive-update-p))

(defun make-buffer-output-stream (&optional (point (current-point))
				    interactive-update-p)
  (make-buffer-stream-instance 'buffer-output-stream
                               point interactive-update-p))

(defmethod trivial-gray-streams::close ((stream buffer-output-stream) &key abort)
  (declare (ignore abort))
  (delete-point (buffer-stream-point stream))
  t)

(defmethod stream-element-type ((stream buffer-output-stream))
  'line)

(defmethod trivial-gray-streams:stream-line-column ((stream buffer-output-stream))
  (point-charpos (buffer-stream-point stream)))

(defun buffer-output-stream-refresh (stream)
  (when (buffer-output-stream-interactive-update-p stream)
    (let ((buffer (point-buffer (buffer-stream-point stream)))
          (point (buffer-stream-point stream)))
      (display-buffer buffer)
      (dolist (window (get-buffer-windows buffer))
        (move-point (buffer-point (window-buffer window)) point))
      (redraw-display)))
  nil)

(defmethod trivial-gray-streams:stream-fresh-line ((stream buffer-output-stream))
  (unless (zerop (point-charpos (buffer-stream-point stream)))
    (trivial-gray-streams:stream-terpri stream)))

(defmethod trivial-gray-streams:stream-write-byte ((stream buffer-output-stream) byte)
  (trivial-gray-streams:stream-write-char stream (code-char byte)))

(defmethod trivial-gray-streams:stream-write-char ((stream buffer-output-stream) char)
  (prog1 char
    (insert-char/point (buffer-stream-point stream)
		       char)))

(defun %write-string-to-buffer-stream (stream string start end &key)
  (insert-string/point (buffer-stream-point stream)
		       (subseq string start end))
  string)

(defun %write-octets-to-buffer-stream (stream octets start end &key)
  (let ((octets (subseq octets start end)))
    (loop :for c :across octets :do
       (trivial-gray-streams:stream-write-byte stream c))
    octets))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream buffer-output-stream)
     sequence start end &key)
  (etypecase sequence
    (string
     (%write-string-to-buffer-stream stream sequence start end))
    ((array (unsigned-byte 8) (*))
     (%write-octets-to-buffer-stream stream sequence start end))))

(defmethod trivial-gray-streams:stream-write-string
    ((stream buffer-output-stream)
     (string string)
     &optional (start 0) end)
  (%write-string-to-buffer-stream stream string start end))

(defmethod trivial-gray-streams:stream-terpri ((stream buffer-output-stream))
  (prog1 (insert-char/point (buffer-stream-point stream)
			    #\newline)
    (buffer-output-stream-refresh stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream buffer-output-stream))
  (buffer-output-stream-refresh stream))

(defmethod trivial-gray-streams:stream-force-output ((stream buffer-output-stream))
  (buffer-output-stream-refresh stream))

#+nil
(defmethod trivial-gray-streams:clear-output ((stream buffer-output-stream))
  )


(defclass minibuffer-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((queue
    :initform nil
    :initarg :queue
    :accessor minibuffer-input-stream-queue)))

(defun make-minibuffer-input-stream ()
  (make-instance 'minibuffer-input-stream :queue nil))

(defmethod trivial-gray-streams:stream-read-char ((stream minibuffer-input-stream))
  (let ((c (pop (minibuffer-input-stream-queue stream))))
    (cond ((null c)
           (let ((string
                  (handler-case (values (minibuf-read-string "") t)
                    (editor-abort ()
		      (setf (minibuffer-input-stream-queue stream) nil)
		      (return-from trivial-gray-streams:stream-read-char :eof)))))
             (setf (minibuffer-input-stream-queue stream)
                   (nconc (minibuffer-input-stream-queue stream)
                          (coerce string 'list)
                          (list #\newline))))
           (trivial-gray-streams:stream-read-char stream))
          ((eql c #\eot)
           :eof)
          (c))))

(defmethod trivial-gray-streams:stream-unread-char ((stream minibuffer-input-stream) char)
  (push char (minibuffer-input-stream-queue stream))
  nil)

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream minibuffer-input-stream))
  (trivial-gray-streams:stream-read-char stream))

(defmethod trivial-gray-streams:stream-peek-char ((stream minibuffer-input-stream))
  (let ((c (trivial-gray-streams:stream-read-char stream)))
    (prog1 c
      (trivial-gray-streams:stream-unread-char stream c))))

(defmethod trivial-gray-streams:stream-listen ((stream minibuffer-input-stream))
  (let ((c (trivial-gray-streams:stream-read-char-no-hang stream)))
    (prog1 c
      (trivial-gray-streams:stream-unread-char stream c))))

(defmethod trivial-gray-streams:stream-read-line ((stream minibuffer-input-stream))
  (minibuf-read-string ""))

(defmethod trivial-gray-streams:stream-clear-input ((stream minibuffer-input-stream))
  nil)

(defclass editor-io-stream (buffer-output-stream minibuffer-input-stream)
  ())

(defun make-editor-io-stream (point &optional interactive-update-p)
  (make-buffer-stream-instance 'editor-io-stream
                               point interactive-update-p))
