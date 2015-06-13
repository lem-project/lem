(in-package :lem)

(defvar *buffer-list* nil)

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

(defun make-buffer (textbuf nlines ncols y x)
  (let ((buffer
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
          :max-col 0)))
    (setq *buffer-list* (append *buffer-list* (list buffer)))
    buffer))

(add-command 'buffer-unmark 'unmark-buffer "M-~")
(defun buffer-unmark (buffer arg)
  (declare (ignore arg))
  (setf (textbuf-modif-p (buffer-textbuf buffer)) nil)
  t)

(defun buffer-change-textbuf (buffer textbuf)
  (let ((old-tb (buffer-textbuf buffer)))
    (setf (textbuf-keep-binfo old-tb)
      (list (buffer-vtop-linum buffer)
        (buffer-cur-linum buffer)
        (buffer-cur-col buffer)
        (buffer-max-col buffer))))
  (setf (buffer-textbuf buffer) textbuf)
  (let ((vtop-linum 1)
        (cur-linum 1)
        (cur-col 0)
        (max-col 0))
    (when (textbuf-keep-binfo textbuf)
      (multiple-value-setq
       (vtop-linum cur-linum cur-col max-col)
       (apply 'values (textbuf-keep-binfo textbuf))))
    (setf (buffer-vtop-linum buffer) vtop-linum)
    (setf (buffer-cur-linum buffer) cur-linum)
    (setf (buffer-cur-col buffer) cur-col)
    (setf (buffer-max-col buffer) max-col)))

(defun buffer-head-line-p (buffer linum)
  (values (<= linum 1) (- 1 linum)))

(defun buffer-tail-line-p (buffer linum)
  (let ((nlines (textbuf-nlines (buffer-textbuf buffer))))
    (values (<= nlines linum) (- nlines linum))))

(defun buffer-bolp (buffer)
  (zerop (buffer-cur-col buffer)))

(defun buffer-eolp (buffer)
  (= (buffer-cur-col buffer)
     (textbuf-line-length
      (buffer-textbuf buffer)
      (buffer-cur-linum buffer))))

(defun buffer-bobp (buffer)
  (and (buffer-head-line-p buffer (buffer-cur-linum buffer))
       (buffer-bolp buffer)))

(defun buffer-eobp (buffer)
  (and (buffer-tail-line-p
	buffer
	(buffer-cur-linum buffer))
       (buffer-eolp buffer)))

(defun buffer-insert-char (buffer c arg)
  (arg-repeat (arg)
    (textbuf-insert-char (buffer-textbuf buffer)
			 (buffer-cur-linum buffer)
			 (buffer-cur-col buffer)
			 c)
    (buffer-next-char buffer 1))
  t)

(add-command 'buffer-insert-newline 'newline "C-j")
(defun buffer-insert-newline (buffer arg)
  (arg-repeat (arg)
    (textbuf-insert-newline (buffer-textbuf buffer)
			    (buffer-cur-linum buffer)
			    (buffer-cur-col buffer))
    (buffer-next-line buffer 1))
  t)

(add-command 'buffer-delete-char 'delete-char "C-d")
(defun buffer-delete-char (buffer arg)
  (arg-repeat (arg t)
    (when (textbuf-delete-char (buffer-textbuf buffer)
			       (buffer-cur-linum buffer)
			       (buffer-cur-col buffer))
      (return nil))))

(defun buffer-set-col (buffer col)
  (setf (buffer-cur-col buffer) col)
  (setf (buffer-max-col buffer) col))

(add-command 'buffer-bol 'beginning-of-line "C-a")
(defun buffer-bol (buffer arg)
  (declare (ignore arg))
  (buffer-set-col buffer 0))

(add-command 'buffer-eol 'end-of-line "C-e")
(defun buffer-eol (buffer arg)
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

(add-command 'buffer-next-line 'next-line "C-n")
(defun buffer-next-line (buffer arg)
  (if (arg-repeat (arg t)
        (if (buffer-tail-line-p buffer (buffer-cur-linum buffer))
          (return)
          (incf (buffer-cur-linum buffer))))
    (progn (%buffer-adjust-col buffer arg) t)
    (progn (buffer-bol buffer nil) t)))

(add-command 'buffer-prev-line 'prev-line "C-p")
(defun buffer-prev-line (buffer arg)
  (if (arg-repeat (arg t)
        (if (buffer-head-line-p buffer (buffer-cur-linum buffer))
          (return)
          (decf (buffer-cur-linum buffer))))
    (progn (%buffer-adjust-col buffer arg) t)
    (progn (buffer-bol buffer nil) nil)))

(add-command 'buffer-next-char 'next-char "C-f")
(defun buffer-next-char (buffer arg)
  (arg-repeat (arg t)
    (cond
      ((buffer-eobp buffer)
       (return nil))
      ((buffer-eolp buffer)
       (buffer-next-line buffer 1))
      (t
       (buffer-set-col buffer (1+ (buffer-cur-col buffer)))))))

(add-command 'buffer-prev-char 'prev-char "C-b")
(defun buffer-prev-char (buffer arg)
  (arg-repeat (arg t)
    (cond
      ((buffer-bobp buffer)
       (return nil))
      ((buffer-bolp buffer)
       (buffer-prev-line buffer 1)
       (buffer-eol buffer 1))
      (t
       (buffer-set-col buffer (1- (buffer-cur-col buffer)))))))
