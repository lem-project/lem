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
  mark-linum
  mark-col
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

(defun buffer-take-lines (buffer linum len)
  (let ((strings))
    (do ((line (buffer-get-line buffer linum) (line-next line))
	 (i 0 (1+ i)))
	((or (null line) (= i len)))
      (push (line-str line) strings))
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
        (acc "")
        (result t))
    (loop while (plusp n) do
      (cond
       ((<= n (- (length (line-str line)) col))
        (setf (buffer-modified-p buffer) t)
        (setq acc
          (format nil "~a~a" 
            acc
            (subseq (line-str line) col (+ col n))))
        (setf (line-str line)
          (concatenate 'string
            (subseq (line-str line) 0 col)
            (subseq (line-str line) (+ col n))))
        (setq n 0))
       (t
        (setq acc
          (format nil "~a~a~%"
            acc
            (subseq (line-str line) col)))
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
    (values result acc)))

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
