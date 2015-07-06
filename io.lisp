(in-package :lem)

(export '(buffer-output-stream
          make-buffer-output-stream
          minibuffer-input-stream
          make-minibuffer-input-stream))

(defclass buffer-output-stream (sb-gray:fundamental-output-stream)
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

(defmethod stream-element-type ((stream buffer-output-stream))
  'line)

(defmethod sb-gray:stream-line-column ((stream buffer-output-stream))
  nil)

(defmethod sb-gray:stream-line-length ((stream buffer-output-stream))
  nil)

(defmethod sb-gray:stream-fresh-line ((stream buffer-output-stream))
  (unless (zerop (buffer-output-stream-column stream))
    (sb-gray:stream-terpri stream)))

(defmethod sb-gray:stream-write-byte ((stream buffer-output-stream) byte)
  (sb-gray:stream-write-char stream (code-char byte)))

(defmethod sb-gray:stream-write-char ((stream buffer-output-stream) char)
  (prog1 (buffer-insert-char (buffer-output-stream-buffer stream)
                             (buffer-output-stream-linum stream)
                             (buffer-output-stream-column stream)
                             char)
    (if (char= char #\newline)
      (progn
        (incf (buffer-output-stream-linum stream))
        (setf (buffer-output-stream-column stream) 0))
      (incf (buffer-output-stream-column stream)))))

(defmethod sb-gray:stream-write-sequence ((stream buffer-output-stream)
                                          sequence &optional (start 0) end)
  (map (type-of sequence)
       (lambda (c)
         (sb-gray:stream-write-byte stream c))
       (subseq sequence start end)))

(defmethod sb-gray:stream-write-string ((stream buffer-output-stream)
                                        (string string)
                                        &optional (start 0) end)
  (map 'string
       (lambda (c)
         (sb-gray:stream-write-char stream c))
       (subseq string start end)))

(defmethod sb-gray:stream-terpri ((stream buffer-output-stream))
  (prog1 (buffer-insert-newline (buffer-output-stream-buffer stream)
                                (buffer-output-stream-linum stream)
                                (buffer-output-stream-column stream))
    (incf (buffer-output-stream-linum stream))
    (setf (buffer-output-stream-column stream) 0)))

(defclass minibuffer-input-stream (sb-gray:fundamental-input-stream)
  ((queue
    :initform nil
    :initarg :queue
    :accessor minibuffer-input-stream-queue)))

(defun make-minibuffer-input-stream ()
  (make-instance 'minibuffer-input-stream :queue nil))

(defmethod sb-gray:stream-read-char ((stream minibuffer-input-stream))
  (let ((c (or (pop (minibuffer-input-stream-queue stream))
               (read-char "Read char: "))))
    (if (char= c #\eot)
      :eof
      c)))

(defmethod sb-gray:stream-unread-char ((stream minibuffer-input-stream) char)
  (push char (minibuffer-input-stream-queue stream))
  nil)

(defmethod sb-gray:stream-read-char-no-hang ((stream minibuffer-input-stream))
  (sb-gray:stream-read-char stream))

(defmethod sb-gray:stream-peek-char ((stream minibuffer-input-stream))
  (let ((c (sb-gray:stream-read-char stream)))
    (prog1 c
      (sb-gray:stream-unread-char stream c))))

(defmethod sb-gray:stream-listen ((stream minibuffer-input-stream))
  (let ((c (sb-gray:stream-read-char-no-hang stream)))
    (prog1 c
      (sb-gray:stream-unread-char stream c))))

(defmethod sb-gray:stream-read-line ((stream minibuffer-input-stream))
  (read-string "Read line: "))

(defmethod sb-gray:stream-clear-input ((stream minibuffer-input-stream))
  nil)
