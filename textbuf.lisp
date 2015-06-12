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

(defvar *textbuf-list* nil)

(defstruct (textbuf (:constructor make-textbuf-internal))
  name
  filename
  modif-p
  head-line
  tail-line
  cache-line
  cache-linum
  keep-binfo-f
  (nlines 1))

(defun make-textbuf (name filename)
  (let ((textbuf (make-textbuf-internal :name name :filename filename))
	(line (make-line nil nil "")))
    (setf (textbuf-head-line textbuf) line)
    (setf (textbuf-tail-line textbuf) line)
    (setf (textbuf-cache-line textbuf) line)
    (setf (textbuf-cache-linum textbuf) 1)
    (push textbuf *textbuf-list*)
    textbuf))

(defun %textbuf-get-line (textbuf linum)
  (cond
    ((= linum (textbuf-cache-linum textbuf))
     (textbuf-cache-line textbuf))
    ((> linum (textbuf-cache-linum textbuf))
     (if (< (- linum (textbuf-cache-linum textbuf))
	    (- (textbuf-nlines textbuf) linum))
       (line-forward-n
	(textbuf-cache-line textbuf)
	(- linum (textbuf-cache-linum textbuf)))
       (line-backward-n
	(textbuf-tail-line textbuf)
	(- (textbuf-nlines textbuf) linum))))
    (t
     (if (< (1- linum)
	    (- (textbuf-cache-linum textbuf) linum))
       (line-forward-n
        (textbuf-head-line textbuf)
	(1- linum))
       (line-backward-n
        (textbuf-cache-line textbuf)
	(- (textbuf-cache-linum textbuf) linum))))))

(defun textbuf-get-line (textbuf linum)
  (let ((line (%textbuf-get-line textbuf linum)))
    (setf (textbuf-cache-linum textbuf) linum)
    (setf (textbuf-cache-line textbuf) line)
    line))

(defun textbuf-line-length (textbuf linum)
  (length (line-str (textbuf-get-line textbuf linum))))

(defun textbuf-take-lines (textbuf linum len)
  (let ((strings))
    (do ((line (textbuf-get-line textbuf linum) (line-next line))
	 (i 0 (1+ i)))
	((or (null line) (= i len)))
      (push (line-str line) strings))
    (nreverse strings)))

(defun textbuf-append-line (textbuf str)
  (setf (textbuf-modif-p textbuf) t)
  (let* ((line (textbuf-tail-line textbuf))
	 (newline (make-line line (line-next line) str)))
    (if (and
	 (= 1 (textbuf-nlines textbuf))
	 (zerop (length (line-str (textbuf-head-line textbuf)))))
      (progn
        (setf (textbuf-head-line textbuf) newline)
        (setf (textbuf-cache-line textbuf) newline))
      (incf (textbuf-nlines textbuf)))
    (setf (textbuf-tail-line textbuf) newline)
    t))

(defun textbuf-insert-char (textbuf linum col c)
  (setf (textbuf-modif-p textbuf) t)
  (let ((line (textbuf-get-line textbuf linum)))
    (setf (line-str line)
          (concatenate 'string
	               (subseq (line-str line) 0 col)
		       (string c)
		       (subseq (line-str line) col))))
  t)

(defun textbuf-insert-newline (textbuf linum col)
  (setf (textbuf-modif-p textbuf) t)
  (let ((line (textbuf-get-line textbuf linum)))
    (let ((newline
            (make-line line
                       (line-next line)
                       (subseq (line-str line) col))))
      (when (eq line (textbuf-tail-line textbuf))
        (setf (textbuf-tail-line textbuf) newline))
      (setf (line-str line)
	    (subseq (line-str line) 0 col))))
  (incf (textbuf-nlines textbuf))
  t)

(defun textbuf-delete-char-1 (textbuf linum col)
  (let ((line (textbuf-get-line textbuf linum)))
    (if (>= col (length (line-str line)))
      (unless (eq line (textbuf-tail-line textbuf))
        (setf (line-str line)
              (concatenate 'string
                           (line-str line)
                           (line-str (line-next line))))
        (when (eq (line-next line)
                  (textbuf-tail-line textbuf))
          (setf (textbuf-tail-line textbuf) line))
	(line-free (line-next line))
	(decf (textbuf-nlines textbuf))
	t)
      (progn
        (setf (line-str line)
	      (concatenate 'string
			   (subseq (line-str line) 0 col)
			   (subseq (line-str line) (1+ col))))
	t))))

(defun textbuf-delete-char (textbuf linum col)
  (let ((result (textbuf-delete-char-1 textbuf linum col)))
    (when result
      (setf (textbuf-modif-p textbuf) t)
      result)))
