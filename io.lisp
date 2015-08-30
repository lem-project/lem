(in-package :lem)

(export '(buffer-output-stream
          make-buffer-output-stream
          minibuffer-input-stream
          make-minibuffer-input-stream))

(defclass buffer-output-stream (trivial-gray-streams:fundamental-output-stream)
  ((buffer
    :initarg :buffer
    :accessor buffer-output-stream-buffer)
   (linum
    :initarg :linum
    :accessor buffer-output-stream-linum)
   (column
    :initarg :column
    :accessor buffer-output-stream-column)))

(defun make-buffer-output-stream (buffer &optional point)
  (make-instance 'buffer-output-stream
                 :buffer buffer
                 :linum (if point
                            (point-linum point)
                            1)
                 :column (if point
                             (point-column point)
                             0)))

(defun buffer-output-stream-point (stream)
  (make-point (buffer-output-stream-linum stream)
              (buffer-output-stream-column stream)))

(defmethod stream-element-type ((stream buffer-output-stream))
  'line)

(defmethod trivial-gray-streams:stream-line-column ((stream buffer-output-stream))
  nil)

(defmethod trivial-gray-streams:stream-fresh-line ((stream buffer-output-stream))
  (unless (zerop (buffer-output-stream-column stream))
    (trivial-gray-streams:stream-terpri stream)))

(defmethod trivial-gray-streams:stream-write-byte ((stream buffer-output-stream) byte)
  (trivial-gray-streams:stream-write-char stream (code-char byte)))

(defmethod trivial-gray-streams:stream-write-char ((stream buffer-output-stream) char)
  (prog1 char
    (buffer-insert-char (buffer-output-stream-buffer stream)
                        (buffer-output-stream-linum stream)
                        (buffer-output-stream-column stream)
                        char)
    (if (char= char #\newline)
        (progn
          (incf (buffer-output-stream-linum stream))
          (setf (buffer-output-stream-column stream) 0))
        (incf (buffer-output-stream-column stream)))))

(defun %write-string-to-buffer-stream (stream string start end &key)
  (let ((string (subseq string start end)))
    (loop :for c :across string :do
       (trivial-gray-streams:stream-write-char stream c))
    string))

(defun %write-octets-to-buffer-stream (stream octets start end &key)
  (let ((octets (subseq octets start end)))
    (loop :for c :across octets :do
       (trivial-gray-streams:stream-write-byte stream c))
    octets))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream buffer-output-stream)
     sequence start end &key)
  (print (type-of sequence))
  (etypecase sequence
    (string
     (%write-string-to-buffer-stream stream sequence start end))
    ((array (unsigned-byte 8) (*))
     (%write-octets-to-buffer-stream stream sequence start end))))

(defmethod trivial-gray-streams:stream-write-string
    ((stream buffer-output-stream)
     (string string)
     &optional (start 0) end)
  (map 'string
       (lambda (c)
         (trivial-gray-streams:stream-write-char stream c))
       (subseq string start end)))

(defmethod trivial-gray-streams:stream-terpri ((stream buffer-output-stream))
  (prog1 (buffer-insert-newline (buffer-output-stream-buffer stream)
                                (buffer-output-stream-linum stream)
                                (buffer-output-stream-column stream))
    (window-update-all)
    (incf (buffer-output-stream-linum stream))
    (setf (buffer-output-stream-column stream) 0)))


(defclass minibuffer-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((queue
    :initform nil
    :initarg :queue
    :accessor minibuffer-input-stream-queue)))

(defun make-minibuffer-input-stream ()
  (make-instance 'minibuffer-input-stream :queue nil))

(defmethod trivial-gray-streams:stream-read-char ((stream minibuffer-input-stream))
  (let ((c (or (pop (minibuffer-input-stream-queue stream))
               (minibuf-read-char "Read char: "))))
    (if (char= c #\eot)
        :eof
        c)))

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
  (minibuf-read-string "Read line: "))

(defmethod trivial-gray-streams:stream-clear-input ((stream minibuffer-input-stream))
  nil)
