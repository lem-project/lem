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

(defstruct (buffer (:constructor make-buffer-internal))
  name
  filename
  modified-p
  read-only-p
  head-line
  tail-line
  cache-line
  cache-linum
  (mark-linum 1)
  (mark-col 0)
  keep-binfo
  (nlines 1))

(defmethod print-object ((buffer buffer) stream)
  (format stream "#<BUFFER ~a ~a>"
    (buffer-name buffer)
    (buffer-filename buffer)))

(defun make-buffer (name &key filename read-only-p)
  (let ((buffer (make-buffer-internal
                 :name name
                 :filename filename
                 :read-only-p read-only-p))
	(line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (push buffer *buffer-list*)
    buffer))

(defmacro buffer-read-only-guard (buffer name)
  `(when (buffer-read-only-p ,buffer)
     (write-message "Read only")
     (return-from ,name nil)))

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
  (buffer-read-only-guard buffer buffer-line-string-set)
  (setf (buffer-modified-p buffer) t)
  (when (and (= linum (buffer-mark-linum buffer))
             (< (length str) (buffer-mark-linum buffer)))
    (setf (buffer-mark-linum buffer) (length str)))
  (let ((line (buffer-get-line buffer linum)))
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
  (buffer-read-only-guard buffer buffer-append-line)
  (setf (buffer-modified-p buffer) t)
  (let* ((line (buffer-tail-line buffer))
	 (newline (make-line line (line-next line) str)))
    (if (and
	 (= 1 (buffer-nlines buffer))
	 (zerop (length (line-str (buffer-head-line buffer)))))
      (progn
        (setf (buffer-head-line buffer) newline)
        (setf (buffer-cache-line buffer) newline))
      (incf (buffer-nlines buffer)))
    (setf (buffer-tail-line buffer) newline)
    t))

(defun buffer-insert-char (buffer linum col c)
  (buffer-read-only-guard buffer buffer-insert-char)
  (setf (buffer-modified-p buffer) t)
  (when (and (= linum (buffer-mark-linum buffer))
             (<= col (buffer-mark-col buffer)))
    (incf (buffer-mark-col buffer)))
  (if (char= c #\newline)
    (buffer-insert-newline buffer linum col)
    (let ((line (buffer-get-line buffer linum)))
      (setf (line-str line)
        (concatenate 'string
          (subseq (line-str line) 0 col)
          (string c)
          (subseq (line-str line) col)))
      t)))

(defun buffer-insert-newline (buffer linum col)
  (buffer-read-only-guard buffer buffer-insert-newline)
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
  (buffer-read-only-guard buffer buffer-insert-line)
  (setf (buffer-modified-p buffer) t)
  (when (and (= linum (buffer-mark-linum buffer))
             (<= col (buffer-mark-col buffer)))
    (incf (buffer-mark-col buffer) (length str)))
  (let ((line (buffer-get-line buffer linum)))
    (setf (line-str line)
      (concatenate 'string
        (subseq (line-str line) 0 col)
        str
        (subseq (line-str line) col))))
  t)

(defun buffer-delete-char (buffer linum col n)
  (buffer-read-only-guard buffer buffer-delete-char)
  (let ((line (buffer-get-line buffer linum))
        (del-lines (list ""))
        (result t))
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
    (values result (nreverse del-lines))))

(defun buffer-erase (buffer)
  (buffer-read-only-guard buffer buffer-erase)
  (setf (buffer-modified-p buffer) t)
  (let ((line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)))

(defun buffer-check-marked (buffer)
  (if (buffer-mark-linum buffer)
    t
    (progn
     (write-message "Not mark in this buffer")
     nil)))
