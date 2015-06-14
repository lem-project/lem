(in-package :lem)

(defvar *window-list* nil)

(defvar *current-cols*)
(defvar *current-lines*)

(defun one-window-p ()
  (null (cdr *window-list*)))

(defun window-init ()
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (setq *current-buffer*
        (make-buffer (make-textbuf "main" nil)
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0))
  (setq *window-list* (list *current-buffer*)))

(defun window-offset-view (buffer)
  (let ((vtop-linum (buffer-vtop-linum buffer))
	(nlines (buffer-nlines buffer))
	(linum (buffer-cur-linum buffer)))
    (cond
      ((< #1=(+ vtop-linum nlines -2) linum)
	(- linum #1#))
      ((> vtop-linum linum)
	(- linum vtop-linum))
      (t
	0))))

(defun window-recenter (buffer)
  (setf (buffer-vtop-linum buffer)
        (buffer-cur-linum buffer))
  (window-scroll buffer (floor (buffer-nlines buffer) 2)))

(defun window-scroll (buffer n)
  (incf (buffer-vtop-linum buffer) n)
  (multiple-value-bind (outp offset)
      (buffer-head-line-p buffer (1+ (buffer-vtop-linum buffer)))
    (when outp
      (incf (buffer-vtop-linum buffer))))
  (multiple-value-bind (outp offset)
      (buffer-tail-line-p buffer (buffer-vtop-linum buffer))
    (when outp
      (incf (buffer-vtop-linum buffer)))))

(defun window-adjust-view (buffer recenter)
  (let ((offset (window-offset-view buffer)))
    (unless (zerop offset)
      (if recenter
        (window-recenter buffer)
	(window-scroll buffer offset)))))

(defun window-percentage (buffer)
  (cond
   ((<= (textbuf-nlines (buffer-textbuf buffer))
        (buffer-nlines buffer))
    "All")
   ((= 1 (buffer-vtop-linum buffer))
    "Top")
   ((<= (textbuf-nlines (buffer-textbuf buffer))
      (+ (buffer-vtop-linum buffer) (buffer-nlines buffer))
        )
    "Bot")
   (t
    (format nil "~2d%"
      (floor
       (* 100
	(float
	 (/ (buffer-vtop-linum buffer)
            (textbuf-nlines (buffer-textbuf buffer))))))))))

(defun window-redraw-modeline-1 (buffer bg-char)
  (cl-ncurses:mvwaddstr
   (buffer-win buffer)
   (1- (buffer-nlines buffer))
   0
   (let ((str (format nil "~c~c ~a: ~a (~{~a ~}) "
		bg-char ; read-only-flag
		(if (textbuf-modif-p (buffer-textbuf buffer)) #\* bg-char)
		"Lem"
		(textbuf-name (buffer-textbuf buffer))
		nil)))
     (format nil
       (format nil
	 "~~~d,,,'~ca ~a ~a"
	 (- (buffer-ncols buffer) 7)
	 bg-char
	 (window-percentage buffer)
	 (make-string 2 :initial-element bg-char))
       str))))

(defun window-redraw-modeline (buffer)
  (cl-ncurses:wattron (buffer-win buffer) cl-ncurses:a_reverse)
  (window-redraw-modeline-1 buffer #\-)
  (cl-ncurses:wattroff (buffer-win buffer) cl-ncurses:a_reverse))

(defun window-redraw-line (buffer str y)
  (let ((width (str-width str))
        (cury (- (buffer-cur-linum buffer)
                 (buffer-vtop-linum buffer)))
        (cols (buffer-ncols buffer))
        (curx))
    (when (= cury y)
      (setq curx (str-width str (buffer-cur-col buffer))))
    (cond
      ((< width (buffer-ncols buffer))
        nil)
      ((or (/= cury y)
           (< curx (1- cols)))
	(let ((i (wide-index str (1- cols))))
          (setq str
                (if (<= cols (str-width str i))
                  (format nil "~a $" (subseq str 0 (1- i)))
                  (format nil "~a$" (subseq str 0 i))))))
      ((< (buffer-cur-col buffer) (length str))
        (let* ((begin (wide-index str (- curx cols -3)))
	       (end (1+ (buffer-cur-col buffer)))
	       (substr (subseq str begin end)))
          (setq str
	        (if (<= cols (+ (str-width substr) 2))
	          (format nil "$~a$" substr)
	          (format nil "$~a $" substr))))
        (setq curx (- cols 2)))
      (t
        (setq str
              (format nil
                      "$~a"
                      (substring-width str (- curx cols -2))))
        (setq curx (- cols 1))))
    (cl-ncurses:mvwaddstr (buffer-win buffer) y 0 str)
    curx))

(defun window-redraw-lines (buffer)
  (let (x)
    (do ((lines
           (textbuf-take-lines (buffer-textbuf buffer)
                               (buffer-vtop-linum buffer)
                               (1- (buffer-nlines buffer)))
           (cdr lines))
         (y 0 (1+ y)))
        ((null lines))
      (let ((curx (window-redraw-line buffer (car lines) y)))
        (when curx
          (setq x curx))))
    (cl-ncurses:wmove (buffer-win buffer)
                      (- (buffer-cur-linum buffer) (buffer-vtop-linum buffer))
                      x)))

(defun window-redraw (buffer)
  (cl-ncurses:werase (buffer-win buffer))
  (window-redraw-modeline buffer)
  (window-redraw-lines buffer)
  (cl-ncurses:wrefresh (buffer-win buffer)))

(defun window-update (buffer)
  (window-adjust-view buffer nil)
  (window-redraw buffer))

(defun window-update-all ()
  (dolist (b *window-list*)
    (unless (eq b *current-buffer*)
      (window-update b)))
  (window-update *current-buffer*))

(add-command 'window-vsplit 'split-window-vertical "C-x2")
(defun window-vsplit (buffer arg)
  (declare (ignore arg))
  (multiple-value-bind (nlines rem)
      (floor (buffer-nlines buffer) 2)
    (let ((newbuf (make-buffer
                   (buffer-textbuf buffer)
                   nlines
                   (buffer-ncols buffer)
                   (+ (buffer-y buffer)
                     nlines
                     rem)
                   (buffer-x buffer))))
      (decf (buffer-nlines buffer) nlines)
      (cl-ncurses:wresize
       (buffer-win buffer)
       (buffer-nlines buffer)
       (buffer-ncols buffer))
      (setf (buffer-vtop-linum newbuf)
            (buffer-vtop-linum buffer))
      (setf (buffer-cur-linum newbuf)
            (buffer-cur-linum buffer))
      (setf (buffer-cur-col newbuf)
            (buffer-cur-col buffer))
      (setf (buffer-max-col newbuf)
            (buffer-max-col buffer))
      (setq *window-list*
        (sort (copy-list (append *window-list* (list newbuf)))
          (lambda (b1 b2)
            (< (buffer-y b1) (buffer-y b2)))))))
  t)

(defun get-next-window (buffer)
  (let ((result (member buffer *window-list*)))
    (if (cdr result)
      (cadr result)
      (car *window-list*))))

(add-command 'window-next 'other-window "C-xo")
(defun window-next (buffer arg)
  (arg-repeat (arg t)
    (setq buffer (get-next-window buffer)))
  (setq *current-buffer* buffer)
  t)

(defun window-move (buffer y x)
  (cl-ncurses:mvwin (buffer-win buffer) y x)
  (setf (buffer-y buffer) y)
  (setf (buffer-x buffer) x))

(defun window-resize (buffer nlines ncols)
  (cl-ncurses:wresize (buffer-win buffer) nlines ncols)
  (setf (buffer-nlines buffer) nlines)
  (setf (buffer-ncols buffer) ncols))

(add-command 'window-delete-other-windows 'delete-other-windows "C-x1")
(defun window-delete-other-windows (buffer arg)
  (declare (ignore arg))
  (dolist (b *window-list*)
    (unless (eq b buffer)
      (cl-ncurses:delwin (buffer-win b))))
  (setq *window-list* (list buffer))
  (window-move buffer 0 0)
  (window-resize buffer
    (1- cl-ncurses:*lines*)
    cl-ncurses:*cols*)
  t)

(add-command 'window-delete 'delete-window "C-x0")
(defun window-delete (buffer arg)
  (declare (ignore arg))
  (cond
   ((one-window-p)
    (mb-write "Can not delete this window")
    nil)
   (t
    (when (eq *current-buffer* buffer)
      (window-next buffer nil))
    (cl-ncurses:delwin (buffer-win buffer))
    (let ((blist (reverse *window-list*)))
      (let ((upbuf (cadr (member buffer blist))))
        (when (null upbuf)
          (setq upbuf (cadr *window-list*))
          (window-move upbuf 0 (buffer-x upbuf)))
        (window-resize upbuf
          (+ (buffer-nlines upbuf)
            (buffer-nlines buffer))
          (buffer-ncols upbuf))))
    (setq *window-list* (delete buffer *window-list*))
    t)))

(defun window-adjust-all ()
  (dolist (b *window-list*)
    (window-resize b
      (buffer-nlines b)
      cl-ncurses:*cols*))
  (dolist (b *window-list*)
    (when (<= cl-ncurses:*lines* (+ 2 (buffer-y b)))
      (window-delete b nil)))
  (let ((b (car (last *window-list*))))
    (window-resize b
      (+ (buffer-nlines b)
        (- cl-ncurses:*lines* *current-lines*))
      (buffer-ncols b)))
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (window-update-all))
