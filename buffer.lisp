(in-package :lem)

(defstruct (line (:constructor make-line-internal))
  (prev nil)
  (str "")
  (next nil))

(defun make-line (prev next str)
  (let ((line (make-line-internal :next next :prev prev :str str)))
    (when next
      (setf (line-prev next) line))
    (when prev
      (setf (line-next prev) line))
    line))

(defun line-free (line)
  (when (line-prev line)
    (setf (line-next (line-prev line))
          (line-next line)))
  (when (line-next line)
    (setf (line-prev (line-next line))
          (line-prev line)))
  (setf (line-prev line) nil
        (line-next line) nil
        (line-str line) nil))

(defun line-step-n (line n step-f)
  (do ((l line (funcall step-f l))
       (i 0 (1+ i)))
      ((= i n) l)))

(defun line-forward-n (line n)
  (line-step-n line n 'line-next))

(defun line-backward-n (line n)
  (line-step-n line n 'line-prev))

(defun line-debug-print (head tail)
  (let ((lines))
    (do ((line head (line-next line)))
        ((null line))
      (push (line-str line) lines))
    (pdebug (nreverse lines)))
  (let ((lines))
    (do ((line tail (line-prev line)))
        ((null line))
      (push (line-str line) lines))
    (pdebug (nreverse lines))))

(define-class buffer () (window-buffer)
  name
  filename
  modified-p
  read-only-p
  major-mode
  head-line
  tail-line
  cache-line
  cache-linum
  mark-linum
  mark-col
  keep-binfo
  nlines
  undo-stack
  redo-stack
  )

(defvar *use-undo-stack* t)
(defvar *use-redo-stack* nil)

(defun make-buffer (name &key filename read-only-p)
  (let ((buffer (make-instance 'buffer
                               :name name
                               :filename filename
                               :read-only-p read-only-p))
	(line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-mark-linum buffer) 1)
    (setf (buffer-mark-col buffer) 0)
    (setf (buffer-nlines buffer) 1)
    (setf (buffer-undo-stack buffer) (make-tlist))
    (setf (buffer-redo-stack buffer) nil)
    (push buffer *buffer-list*)
    buffer))

(defmethod print-object ((buffer buffer) stream)
  (format stream "#<BUFFER ~a ~a>"
    (buffer-name buffer)
    (buffer-filename buffer)))

(defun push-undo-stack (buffer elt)
  (tlist-add-right (buffer-undo-stack buffer) elt))

(defun push-redo-stack (buffer elt)
  (push elt (buffer-redo-stack buffer)))

(defmacro with-push-undo ((buffer) &body body)
  (let ((gmark-linum (gensym "MARK-LINUM"))
        (gmark-col (gensym "MARK-COL")))
    `(when (or *use-undo-stack*
               *use-redo-stack*)
       (let ((,gmark-linum (buffer-mark-linum ,buffer))
             (,gmark-col (buffer-mark-col ,buffer)))
         (let ((elt (lambda ()
                      (setf (buffer-mark-col ,buffer) ,gmark-col)
                      (setf (buffer-mark-linum ,buffer) ,gmark-linum)
                      ,@body)))
           (if *use-redo-stack*
             (push-redo-stack ,buffer elt)
             (push-undo-stack ,buffer elt)))))))

(defmacro buffer-read-only-guard (buffer)
  `(when (buffer-read-only-p ,buffer)
     (throw 'abort 'readonly)))

(defun %buffer-get-line (buffer linum)
  (cond
    ((= linum (buffer-cache-linum buffer))
     (buffer-cache-line buffer))
    ((> linum (buffer-cache-linum buffer))
     (if (< (- linum (buffer-cache-linum buffer))
	    (- (buffer-nlines buffer) linum))
       (line-forward-n
	(buffer-cache-line buffer)
	(- linum (buffer-cache-linum buffer)))
       (line-backward-n
	(buffer-tail-line buffer)
	(- (buffer-nlines buffer) linum))))
    (t
     (if (< (1- linum)
	    (- (buffer-cache-linum buffer) linum))
       (line-forward-n
        (buffer-head-line buffer)
	(1- linum))
       (line-backward-n
        (buffer-cache-line buffer)
	(- (buffer-cache-linum buffer) linum))))))

(defun buffer-get-line (buffer linum)
  (let ((line (%buffer-get-line buffer linum)))
    (setf (buffer-cache-linum buffer) linum)
    (setf (buffer-cache-line buffer) line)
    line))

(defun buffer-get-char (buffer linum column)
  (let ((line (buffer-get-line buffer linum)))
    (when (line-p line)
      (let ((str (line-str line)))
        (cond
         ((<= 0 column (1- (length str)))
          (aref str column))
         ((= column (length str))
          #\newline))))))

(defun buffer-line-length (buffer linum)
  (length (line-str (buffer-get-line buffer linum))))

(defun buffer-line-string (buffer linum)
  (let ((line (buffer-get-line buffer linum)))
    (when (line-p line)
      (line-str line))))

(defun buffer-line-string-set (buffer linum str)
  (buffer-read-only-guard buffer)
  (let ((line (buffer-get-line buffer linum))
        (mark-linum (buffer-mark-linum buffer))
        (mark-col (buffer-mark-col buffer)))
    (let ((oldstr (line-str line))
          (old-modified-p (buffer-modified-p buffer)))
      (with-push-undo (buffer)
        (buffer-line-string-set buffer
                                linum
                                oldstr)
        (setf (buffer-modified-p buffer) old-modified-p)
        (make-point linum (length oldstr))))
    (setf (buffer-modified-p buffer) t)
    (when (and (= linum (buffer-mark-linum buffer))
               (< (length str) (buffer-mark-linum buffer)))
      (setf (buffer-mark-linum buffer) (length str)))
    (when (line-p line)
      (setf (line-str line) str))))

(defun map-buffer-lines (fn buffer &optional start end)
  (let ((head-line
         (if start
           (buffer-get-line buffer start)
           (buffer-head-line buffer))))
    (unless end
      (setq end (buffer-nlines buffer)))
    (do ((line head-line (line-next line))
         (i (or start 1) (1+ i)))
        ((or (null line) (< end i)))
      (funcall fn
        (line-str line)
        (if (line-next line) nil t)
        i))))

(defun buffer-take-lines (buffer &optional linum len)
  (unless linum
    (setq linum 1))
  (unless len
    (setq len (buffer-nlines buffer)))
  (let ((strings))
    (map-buffer-lines
     (lambda (str eof-p linum)
       (declare (ignore eof-p linum))
       (push str strings))
     buffer
     linum
     (+ linum len -1))
    (nreverse strings)))

(defun buffer-append-line (buffer str)
  (buffer-read-only-guard buffer)
  (setf (buffer-modified-p buffer) t)
  (let* ((line (buffer-tail-line buffer))
	 (newline (make-line line (line-next line) str)))
    (cond ((and (= 1 (buffer-nlines buffer))
                (zerop (length (line-str (buffer-head-line buffer)))))
           (setf (buffer-head-line buffer) newline)
           (setf (buffer-cache-line buffer) newline))
          (t
           (incf (buffer-nlines buffer))))
    (setf (buffer-tail-line buffer) newline)
    t))

(defun buffer-insert-char (buffer linum col c)
  (buffer-read-only-guard buffer)
  (cond ((char= c #\newline)
         (buffer-insert-newline buffer linum col))
        (t
         (let ((old-modified-p (buffer-modified-p buffer)))
           (with-push-undo (buffer)
             (buffer-delete-char buffer linum col 1)
             (setf (buffer-modified-p buffer) old-modified-p)
             (make-point linum col))
           (setf (buffer-modified-p buffer) t)
           (let ((line (buffer-get-line buffer linum)))
             (when (and (= linum (buffer-mark-linum buffer))
                        (<= col (buffer-mark-col buffer)))
               (incf (buffer-mark-col buffer)))
             (setf (line-str line)
                   (concatenate 'string
                                (subseq (line-str line) 0 col)
                                (string c)
                                (subseq (line-str line) col)))))))
  t)

(defun buffer-insert-newline (buffer linum col)
  (buffer-read-only-guard buffer)
  (let ((old-modified-p (buffer-modified-p buffer)))
    (with-push-undo (buffer)
      (buffer-delete-char buffer linum col 1)
      (setf (buffer-modified-p buffer) old-modified-p)
      (make-point linum col)))
  (setf (buffer-modified-p buffer) t)
  (cond
   ((= (buffer-mark-linum buffer) linum)
    (incf (buffer-mark-linum buffer))
    (decf (buffer-mark-col buffer) col))
   ((< linum (buffer-mark-linum buffer))
    (incf (buffer-mark-linum buffer))))
  (let ((line (buffer-get-line buffer linum)))
    (let ((newline
           (make-line line
                      (line-next line)
                      (subseq (line-str line) col))))
      (when (eq line (buffer-tail-line buffer))
        (setf (buffer-tail-line buffer) newline))
      (setf (line-str line)
            (subseq (line-str line) 0 col))))
  (incf (buffer-nlines buffer))
  t)

(defun buffer-insert-line (buffer linum col str)
  (buffer-read-only-guard buffer)
  (let ((line (buffer-get-line buffer linum)))
    (let ((oldstr (line-str line))
          (old-modified-p (buffer-modified-p buffer)))
      (with-push-undo (buffer)
        (let ((line (buffer-get-line buffer linum)))
          (setf (line-str line) oldstr)
          (setf (buffer-modified-p buffer) old-modified-p)
          (make-point linum col))))
    (setf (buffer-modified-p buffer) t)
    (when (and (= linum (buffer-mark-linum buffer))
               (<= col (buffer-mark-col buffer)))
      (incf (buffer-mark-col buffer) (length str)))
    (setf (line-str line)
      (concatenate 'string
        (subseq (line-str line) 0 col)
        str
        (subseq (line-str line) col))))
  t)

(defun buffer-delete-char (buffer linum col n)
  (buffer-read-only-guard buffer)
  (let ((line (buffer-get-line buffer linum))
        (del-lines (list ""))
        (result t)
        (old-modified-p (buffer-modified-p buffer)))
    (loop while (plusp n) do
      (cond
       ((<= n (- (length (line-str line)) col))
        (when (and (= linum (buffer-mark-linum buffer))
                   (< col (buffer-mark-col buffer)))
          (setf (buffer-mark-col buffer)
            (if (> col (- (buffer-mark-col buffer) n))
              col
              (- (buffer-mark-col buffer) n))))
        (setf (buffer-modified-p buffer) t)
        (setf (car del-lines)
          (concatenate 'string
            (car del-lines)
            (subseq (line-str line) col (+ col n))))
        (setf (line-str line)
          (concatenate 'string
            (subseq (line-str line) 0 col)
            (subseq (line-str line) (+ col n))))
        (setq n 0))
       (t
        (cond
         ((and (= linum (buffer-mark-linum buffer))
               (< col (buffer-mark-col buffer)))
          (setf (buffer-mark-col buffer) col))
         ((< linum (buffer-mark-linum buffer))
          (decf (buffer-mark-linum buffer))))
        (setf (car del-lines)
          (concatenate 'string
            (car del-lines)
            (subseq (line-str line) col)))
        (push "" del-lines)
        (unless (line-next line)
          (setq result nil)
          (return nil))
        (decf n (1+ (- (length (line-str line)) col)))
        (decf (buffer-nlines buffer))
        (setf (buffer-modified-p buffer) t)
        (setf (line-str line)
          (concatenate 'string
            (subseq (line-str line) 0 col)
            (line-str (line-next line))))
        (when (eq (line-next line)
                  (buffer-tail-line buffer))
          (setf (buffer-tail-line buffer) line))
        (line-free (line-next line)))))
    (setq del-lines (nreverse del-lines))
    (with-push-undo (buffer)
      (let ((linum linum)
            (col col))
        (do ((rest del-lines (cdr rest)))
            ((null rest))
          (buffer-insert-line buffer linum col (car rest))
          (when (cdr rest)
            (buffer-insert-newline buffer linum (+ col (length (car rest))))
            (incf linum)
            (setq col 0)))
        (setf (buffer-modified-p buffer) old-modified-p)
        (make-point linum col)))
    (values result del-lines)))

(defun buffer-erase (buffer)
  (buffer-read-only-guard buffer)
  (setf (buffer-modified-p buffer) t)
  (let ((line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)
    (setf (buffer-undo-stack buffer) (make-tlist))
    (setf (buffer-redo-stack buffer) nil)))

(defun buffer-check-marked (buffer)
  (if (buffer-mark-linum buffer)
    t
    (progn
     (write-message "Not mark in this buffer")
     nil)))

(defun buffer-undo (buffer)
  (let ((tlist (buffer-undo-stack buffer)))
    (unless (tlist-empty-p tlist)
      (let ((f (tlist-right tlist))
            (*use-redo-stack* t))
        (setf (car tlist) (butlast (car tlist)))
        (tlist-update tlist)
        (funcall f)))))

(defun buffer-redo (buffer)
  (let ((f (pop (buffer-redo-stack buffer))))
    (when f
      (let ((*use-undo-stack* t))
        (funcall f)))))

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
