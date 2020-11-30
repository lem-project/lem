(defpackage :lem-lsp-mode/async-process-stream
  (:use :cl
        :trivial-gray-streams)
  (:export :make-input-stream))
(in-package :lem-lsp-mode/async-process-stream)

(defclass input-stream (fundamental-character-input-stream)
  ((process :initarg :process
            :reader input-stream-process)
   (buffer-string :initform ""
                  :accessor input-stream-buffer-string)
   (position :initform 0
             :accessor input-stream-position)))

(defun make-input-stream (process)
  (make-instance 'input-stream :process process))

(defun receive-output-if-necessary (stream)
  (when (<= (length (input-stream-buffer-string stream))
            (input-stream-position stream))
    (setf (input-stream-buffer-string stream)
          (async-process:process-receive-output (input-stream-process stream)))
    (setf (input-stream-position stream)
          0)))

(defun ahead-char (stream)
  (char (input-stream-buffer-string stream)
        (input-stream-position stream)))

(defmethod stream-read-char ((stream input-stream))
  (receive-output-if-necessary stream)
  (prog1 (ahead-char stream)
    (incf (input-stream-position stream))))

(defmethod stream-unread-char ((stream input-stream) character)
  (decf (input-stream-position stream))
  nil)

#+(or)
(defmethod stream-read-char-no-hang ((stream input-stream))
  )

(defmethod stream-peek-char ((stream input-stream))
  ;; TODO: プロセスが終了している場合は:EOFを返す?
  (receive-output-if-necessary stream)
  (ahead-char stream))

(defmethod stream-listen ((stream input-stream))
  t)

(defmethod stream-read-line ((stream input-stream))
  (receive-output-if-necessary stream)
  (let ((pos (position #\newline
                       (input-stream-buffer-string stream)
                       :start (input-stream-position stream))))
    (prog1 (subseq (input-stream-buffer-string stream)
                   (input-stream-position stream)
                   pos)
      (setf (input-stream-position stream)
            (or (if pos
                    (1+ pos)
                    (length (input-stream-buffer-string stream))))))))

(defmethod stream-clear-input ((stream input-stream))
  nil)
