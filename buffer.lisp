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

(defstruct (buffer (:constructor make-buffer-internal))
  name
  filename
  modified-p
  head-line
  tail-line
  cache-line
  cache-linum
  keep-binfo
  (nlines 1))

(defmethod print-object ((buffer buffer) stream)
  (format stream "#<BUFFER ~a ~a>"
    (buffer-name buffer)
    (buffer-filename buffer)))

(defun make-buffer (name filename)
  (let ((buffer (make-buffer-internal :name name :filename filename))
	(line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (push buffer *buffer-list*)
    buffer))

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
  (setf (buffer-modified-p buffer) t)
  (let ((line (buffer-get-line buffer linum)))
    (setf (line-str line)
          (concatenate 'string
	               (subseq (line-str line) 0 col)
		       (string c)
		       (subseq (line-str line) col))))
  t)

(defun buffer-insert-newline (buffer linum col)
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

(defun buffer-delete-char-1 (buffer linum col)
  (let ((line (buffer-get-line buffer linum)))
    (if (>= col (length (line-str line)))
      (unless (eq line (buffer-tail-line buffer))
        (setf (line-str line)
              (concatenate 'string
                           (line-str line)
                           (line-str (line-next line))))
        (when (eq (line-next line)
                  (buffer-tail-line buffer))
          (setf (buffer-tail-line buffer) line))
	(line-free (line-next line))
	(decf (buffer-nlines buffer))
	t)
      (progn
        (setf (line-str line)
	      (concatenate 'string
			   (subseq (line-str line) 0 col)
			   (subseq (line-str line) (1+ col))))
	t))))

(defun buffer-delete-char (buffer linum col)
  (let ((result (buffer-delete-char-1 buffer linum col)))
    (when result
      (setf (buffer-modified-p buffer) t)
      result)))

(defun buffer-erase (buffer)
  (setf (buffer-modified-p buffer) t)
  (let ((line (make-line nil nil "")))
    (setf (buffer-head-line buffer) line)
    (setf (buffer-tail-line buffer) line)
    (setf (buffer-cache-line buffer) line)
    (setf (buffer-cache-linum buffer) 1)
    (setf (buffer-keep-binfo buffer) nil)
    (setf (buffer-nlines buffer) 1)))
