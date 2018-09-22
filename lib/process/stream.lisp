(in-package :lem-process)

(defclass %process-stream ()
  ((process
    :initform (error ":process missing")
    :initarg :process
    :reader stream-process)))


;;; input
(defclass process-input-stream (%process-stream trivial-gray-streams:fundamental-character-input-stream)
  ((read-ahead-point
    :reader stream-read-ahead-point)
   (unread-char
    :initform nil
    :accessor stream-unread-char)))

(defmethod initialize-instance :after ((stream process-input-stream) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value stream 'read-ahead-point)
        (copy-point (buffer-start-point (process-buffer (stream-process stream)))
                    :right-inserting)))

(defmethod trivial-gray-streams::close ((stream process-input-stream) &key abort)
  (declare (ignore abort))
  (delete-point (stream-read-ahead-point stream)))

(defmethod trivial-gray-streams:stream-read-char ((stream process-input-stream))
  (let ((character (stream-unread-char stream))
        (point (stream-read-ahead-point stream)))
    (cond (character
           (setf (stream-unread-char stream) nil)
           character)
          ((end-buffer-p point)
           :eof)
          (t
           (prog1 (character-at point)
             (character-offset point 1))))))

(defmethod trivial-gray-streams:stream-unread-char ((stream process-input-stream) character)
  (setf (stream-unread-char stream) character)
  nil)

#+(or)
(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream process-input-stream))
  )

(defmethod trivial-gray-streams:stream-peek-char ((stream process-input-stream))
  (or (stream-unread-char stream)
      (let ((point (stream-read-ahead-point stream)))
        (if (end-buffer-p point)
            :eof
            (character-at point)))))

(defmethod trivial-gray-streams:stream-listen ((stream process-input-stream))
  (not (end-buffer-p (stream-read-ahead-point stream))))

#+(or)
(defmethod trivial-gray-streams:stream-read-line ((stream process-input-stream))
  )

(defmethod trivial-gray-streams:stream-clear-input ((stream process-input-stream))
  nil)


;;; output
(defclass process-output-stream (%process-stream trivial-gray-streams:fundamental-character-output-stream)
  ((column
    :initform 0
    :accessor stream-column)))

(defmethod trivial-gray-streams:stream-write-char ((stream process-output-stream) character)
  (process-send-input (stream-process stream) (string character))
  (if (char= character #\newline)
      (setf (stream-column stream) 0)
      (setf (stream-column stream)
            (char-width character (stream-column stream)))))

(defmethod trivial-gray-streams:stream-line-column ((stream process-output-stream))
  (stream-column stream))

(defmethod trivial-gray-streams:stream-start-line-p ((stream process-output-stream))
  (zerop (stream-column stream)))

#+(or)
(defmethod trivial-gray-streams:stream-write-string ((stream process-output-stream) string &optional start end)
  )

#+(or)
(defmethod trivial-gray-streams:stream-terpri ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-fresh-line ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-finish-output ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-force-output ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-clear-output ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-advance-to-column ((stream process-output-stream) column)
  )


(defclass process-io-stream (process-input-stream process-output-stream)
  ())
