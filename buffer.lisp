(in-package :lem)

(defstruct (buffer (:constructor make-buffer-internal))
  win
  nlines
  ncols
  y
  x
  textbuf
  vtop-linum
  cur-linum
  cur-col
  max-col)

(defgeneric buffer-unmark (buffer arg))
(defgeneric buffer-change-textbuf (buffer textbuf))
(defgeneric buffer-head-line-p (buffer linum))
(defgeneric buffer-tail-line-p (buffer linum))
(defgeneric buffer-bolp (buffer))
(defgeneric buffer-eolp (buffer))
(defgeneric buffer-bobp (buffer))
(defgeneric buffer-eobp (buffer))
(defgeneric buffer-insert-char (buffer c arg))
(defgeneric buffer-insert-newline (buffer arg))
(defgeneric buffer-delete-char (buffer arg))
(defgeneric buffer-set-col (buffer col))
(defgeneric buffer-bol (buffer arg))
(defgeneric buffer-eol (buffer arg))
(defgeneric buffer-next-line (buffer arg))
(defgeneric buffer-prev-line (buffer arg))
(defgeneric buffer-next-char (buffer arg))
(defgeneric buffer-prev-char (buffer arg))

(defun make-buffer (textbuf nlines ncols y x)
  (make-buffer-internal
    :win (cl-ncurses:newwin nlines ncols y x)
    :nlines nlines
    :ncols ncols
    :y y
    :x x
    :textbuf textbuf
    :vtop-linum 1
    :cur-linum 1
    :cur-col 0
    :max-col 0))

(defmethod buffer-unmark ((buffer buffer) arg)
  (declare (ignore arg))
  (setf (textbuf-modif-p (buffer-textbuf buffer)) nil)
  t)

(defmethod buffer-change-textbuf ((buffer buffer) textbuf)
  (let ((old-tb (buffer-textbuf buffer)))
    (setf (textbuf-keep-binfo-f old-tb)
          (lambda ()
	    (values (buffer-vtop-linum buffer)
	            (buffer-cur-linum buffer)
	            (buffer-cur-col buffer)
	            (buffer-max-col buffer)))))
  (setf (buffer-textbuf buffer) textbuf)
  (let ((vtop-linum 1)
        (cur-linum 1)
        (cur-col 0)
        (max-col 0))
    (when (textbuf-keep-binfo-f textbuf)
      (multiple-value-setq
       (vtop-linum cur-linum cur-col max-col)
       (funcall (textbuf-keep-binfo-f textbuf))))
    (setf (buffer-vtop-linum buffer) vtop-linum)
    (setf (buffer-cur-linum buffer) cur-linum)
    (setf (buffer-cur-col buffer) cur-col)
    (setf (buffer-max-col buffer) max-col)))

(defmethod buffer-head-line-p ((buffer buffer) linum)
  (values (<= linum 1) (- 1 linum)))

(defmethod buffer-tail-line-p ((buffer buffer) linum)
  (let ((nlines (textbuf-nlines (buffer-textbuf buffer))))
    (values (<= nlines linum) (- nlines linum))))

(defmethod buffer-bolp ((buffer buffer))
  (zerop (buffer-cur-col buffer)))

(defmethod buffer-eolp ((buffer buffer))
  (= (buffer-cur-col buffer)
     (textbuf-line-length
      (buffer-textbuf buffer)
      (buffer-cur-linum buffer))))

(defmethod buffer-bobp ((buffer buffer))
  (and (buffer-head-line-p buffer (buffer-cur-linum buffer))
       (buffer-bolp buffer)))

(defmethod buffer-eobp ((buffer buffer))
  (and (buffer-tail-line-p
	buffer
	(buffer-cur-linum buffer))
       (buffer-eolp buffer)))

(defmethod buffer-insert-char ((buffer buffer) c arg)
  (arg-repeat (arg)
    (textbuf-insert-char (buffer-textbuf buffer)
			 (buffer-cur-linum buffer)
			 (buffer-cur-col buffer)
			 c)
    (buffer-next-char buffer 1))
  t)

(defmethod buffer-insert-newline ((buffer buffer) arg)
  (arg-repeat (arg)
    (textbuf-insert-newline (buffer-textbuf buffer)
			    (buffer-cur-linum buffer)
			    (buffer-cur-col buffer))
    (buffer-next-line buffer 1))
  t)

(defmethod buffer-delete-char ((buffer buffer) arg)
  (arg-repeat (arg t)
    (when (textbuf-delete-char (buffer-textbuf buffer)
			       (buffer-cur-linum buffer)
			       (buffer-cur-col buffer))
      (return nil))))

(defmethod buffer-set-col ((buffer buffer) col)
  (setf (buffer-cur-col buffer) col)
  (setf (buffer-max-col buffer) col))

(defmethod buffer-bol ((buffer buffer) arg)
  (declare (ignore arg))
  (buffer-set-col buffer 0))

(defmethod buffer-eol ((buffer buffer) arg)
  (declare (ignore arg))
  (buffer-set-col buffer
		  (textbuf-line-length
		   (buffer-textbuf buffer)
		   (buffer-cur-linum buffer)))
  t)

(defun %buffer-adjust-col (buffer arg)
  (if arg
    (buffer-bol buffer nil)
    (setf (buffer-cur-col buffer)
	  (min (buffer-max-col buffer)
	       (textbuf-line-length
	        (buffer-textbuf buffer)
		(buffer-cur-linum buffer))))))

(defmethod buffer-next-line ((buffer buffer) arg)
  (if (arg-repeat (arg t)
        (if (buffer-tail-line-p buffer (buffer-cur-linum buffer))
          (return)
          (incf (buffer-cur-linum buffer))))
    (progn (%buffer-adjust-col buffer arg) t)
    (progn (buffer-bol buffer nil) t)))

(defmethod buffer-prev-line ((buffer buffer) arg)
  (if (arg-repeat (arg t)
        (if (buffer-head-line-p buffer (buffer-cur-linum buffer))
          (return)
          (decf (buffer-cur-linum buffer))))
    (progn (%buffer-adjust-col buffer arg) t)
    (progn (buffer-bol buffer nil) nil)))

(defmethod buffer-next-char ((buffer buffer) arg)
  (arg-repeat (arg t)
    (cond
      ((buffer-eobp buffer)
       (return nil))
      ((buffer-eolp buffer)
       (buffer-next-line buffer 1))
      (t
       (buffer-set-col buffer (1+ (buffer-cur-col buffer)))))))

(defmethod buffer-prev-char ((buffer buffer) arg)
  (arg-repeat (arg t)
    (cond
      ((buffer-bobp buffer)
       (return nil))
      ((buffer-bolp buffer)
       (buffer-prev-line buffer 1)
       (buffer-eol buffer 1))
      (t
       (buffer-set-col buffer (1- (buffer-cur-col buffer)))))))
