(cl-lsp/defpackage:defpackage :cl-lsp/gray-streams
  (:use :cl :trivial-gray-streams)
  (:export :lsp-output-stream))
(in-package :cl-lsp/gray-streams)

(defclass lsp-output-stream (fundamental-character-output-stream)
  ((output-fn
    :initarg :output-fn
    :reader output-fn)
   (buffer
    :initform (make-string 8000)
    :reader buffer)
   (index
    :initform 0
    :accessor index)
   (column
    :initform 0
    :accessor column)))

(defmethod stream-write-char ((stream lsp-output-stream) character)
  (setf (schar (buffer stream) (index stream))
        character)
  (incf (index stream))
  (if (char= character #\newline)
      (setf (column stream) 0)
      (incf (column stream)))
  (when (= (index stream) (length (buffer stream)))
    (stream-finish-output stream))
  character)

(defmethod stream-write-string ((stream lsp-output-stream) (string string) &optional start end)
  (let* ((start (or start 0))
         (end (or end (length string)))
         (len (length (buffer stream)))
         (count (- end start))
         (free (- len (index stream))))
    (when (>= count free)
      (stream-finish-output stream))
    (cond ((< count len)
           (replace (buffer stream) string
                    :start1 (index stream)
                    :start2 start :end2 end)
           (incf (index stream) count))
          (t
           (funcall (output-fn stream) (subseq string start end))))
    (let ((last-newline (position #\newline string
                                  :from-end t
                                  :start start :end end)))
      (setf (column stream)
            (if last-newline
                (- end last-newline 1)
                (+ (column stream) count)))))
  string)

(defmethod stream-line-column ((stream lsp-output-stream))
  (column stream))

(defmethod stream-finish-output ((stream lsp-output-stream))
  (when (< 0 (index stream))
    (funcall (output-fn stream)
             (subseq (buffer stream)
                     0
                     (index stream)))
    (setf (index stream) 0))
  nil)

(defmethod stream-force-output ((stream lsp-output-stream))
  (stream-finish-output stream))

(defmethod stream-fresh-line ((stream lsp-output-stream))
  (cond ((zerop (column stream))
         nil)
        (t
         (terpri stream)
         t)))
