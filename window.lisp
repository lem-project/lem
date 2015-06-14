(in-package :lem)

(defstruct (window (:constructor make-window-internal))
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

(defun make-window (textbuf nlines ncols y x)
  (let ((buffer
         (make-window-internal
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
    buffer))

(defvar *window-list* nil)

(defvar *current-cols*)
(defvar *current-lines*)

(defun one-window-p ()
  (null (cdr *window-list*)))

(defun window-init ()
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (setq *current-buffer*
        (make-window (make-textbuf "main" nil)
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0))
  (setq *window-list* (list *current-buffer*)))

(defun window-offset-view (buffer)
  (let ((vtop-linum (window-vtop-linum buffer))
	(nlines (window-nlines buffer))
	(linum (window-cur-linum buffer)))
    (cond
      ((< #1=(+ vtop-linum nlines -2) linum)
	(- linum #1#))
      ((> vtop-linum linum)
	(- linum vtop-linum))
      (t
	0))))

(defun window-recenter (buffer)
  (setf (window-vtop-linum buffer)
        (window-cur-linum buffer))
  (window-scroll buffer (floor (window-nlines buffer) 2)))

(defun window-scroll (buffer n)
  (incf (window-vtop-linum buffer) n)
  (multiple-value-bind (outp offset)
      (buffer-head-line-p buffer (1+ (window-vtop-linum buffer)))
    (when outp
      (incf (window-vtop-linum buffer))))
  (multiple-value-bind (outp offset)
      (buffer-tail-line-p buffer (window-vtop-linum buffer))
    (when outp
      (incf (window-vtop-linum buffer)))))

(defun window-adjust-view (buffer recenter)
  (let ((offset (window-offset-view buffer)))
    (unless (zerop offset)
      (if recenter
        (window-recenter buffer)
	(window-scroll buffer offset)))))

(defun window-percentage (buffer)
  (cond
   ((<= (textbuf-nlines (window-textbuf buffer))
        (window-nlines buffer))
    "All")
   ((= 1 (window-vtop-linum buffer))
    "Top")
   ((<= (textbuf-nlines (window-textbuf buffer))
      (+ (window-vtop-linum buffer) (window-nlines buffer))
        )
    "Bot")
   (t
    (format nil "~2d%"
      (floor
       (* 100
	(float
	 (/ (window-vtop-linum buffer)
            (textbuf-nlines (window-textbuf buffer))))))))))

(defun window-redraw-modeline-1 (buffer bg-char)
  (cl-ncurses:mvwaddstr
   (window-win buffer)
   (1- (window-nlines buffer))
   0
   (let ((str (format nil "~c~c ~a: ~a (~{~a ~}) "
		bg-char ; read-only-flag
		(if (textbuf-modif-p (window-textbuf buffer)) #\* bg-char)
		"Lem"
		(textbuf-name (window-textbuf buffer))
		nil)))
     (format nil
       (format nil
	 "~~~d,,,'~ca ~a ~a"
	 (- (window-ncols buffer) 7)
	 bg-char
	 (window-percentage buffer)
	 (make-string 2 :initial-element bg-char))
       str))))

(defun window-redraw-modeline (buffer)
  (cl-ncurses:wattron (window-win buffer) cl-ncurses:a_reverse)
  (window-redraw-modeline-1 buffer #\-)
  (cl-ncurses:wattroff (window-win buffer) cl-ncurses:a_reverse))

(defun window-redraw-line (buffer str y)
  (let ((width (str-width str))
        (cury (- (window-cur-linum buffer)
                 (window-vtop-linum buffer)))
        (cols (window-ncols buffer))
        (curx))
    (when (= cury y)
      (setq curx (str-width str (window-cur-col buffer))))
    (cond
      ((< width (window-ncols buffer))
        nil)
      ((or (/= cury y)
           (< curx (1- cols)))
	(let ((i (wide-index str (1- cols))))
          (setq str
                (if (<= cols (str-width str i))
                  (format nil "~a $" (subseq str 0 (1- i)))
                  (format nil "~a$" (subseq str 0 i))))))
      ((< (window-cur-col buffer) (length str))
        (let* ((begin (wide-index str (- curx cols -3)))
	       (end (1+ (window-cur-col buffer)))
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
    (cl-ncurses:mvwaddstr (window-win buffer) y 0 str)
    curx))

(defun window-redraw-lines (buffer)
  (let (x)
    (do ((lines
           (textbuf-take-lines (window-textbuf buffer)
                               (window-vtop-linum buffer)
                               (1- (window-nlines buffer)))
           (cdr lines))
         (y 0 (1+ y)))
        ((null lines))
      (let ((curx (window-redraw-line buffer (car lines) y)))
        (when curx
          (setq x curx))))
    (cl-ncurses:wmove (window-win buffer)
                      (- (window-cur-linum buffer) (window-vtop-linum buffer))
                      x)))

(defun window-redraw (buffer)
  (cl-ncurses:werase (window-win buffer))
  (window-redraw-modeline buffer)
  (window-redraw-lines buffer)
  (cl-ncurses:wrefresh (window-win buffer)))

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
      (floor (window-nlines buffer) 2)
    (let ((newbuf (make-window
                   (window-textbuf buffer)
                   nlines
                   (window-ncols buffer)
                   (+ (window-y buffer)
                     nlines
                     rem)
                   (window-x buffer))))
      (decf (window-nlines buffer) nlines)
      (cl-ncurses:wresize
       (window-win buffer)
       (window-nlines buffer)
       (window-ncols buffer))
      (setf (window-vtop-linum newbuf)
            (window-vtop-linum buffer))
      (setf (window-cur-linum newbuf)
            (window-cur-linum buffer))
      (setf (window-cur-col newbuf)
            (window-cur-col buffer))
      (setf (window-max-col newbuf)
            (window-max-col buffer))
      (setq *window-list*
        (sort (copy-list (append *window-list* (list newbuf)))
          (lambda (b1 b2)
            (< (window-y b1) (window-y b2)))))))
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
  (cl-ncurses:mvwin (window-win buffer) y x)
  (setf (window-y buffer) y)
  (setf (window-x buffer) x))

(defun window-resize (buffer nlines ncols)
  (cl-ncurses:wresize (window-win buffer) nlines ncols)
  (setf (window-nlines buffer) nlines)
  (setf (window-ncols buffer) ncols))

(add-command 'window-delete-other-windows 'delete-other-windows "C-x1")
(defun window-delete-other-windows (buffer arg)
  (declare (ignore arg))
  (dolist (b *window-list*)
    (unless (eq b buffer)
      (cl-ncurses:delwin (window-win b))))
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
    (cl-ncurses:delwin (window-win buffer))
    (let ((blist (reverse *window-list*)))
      (let ((upbuf (cadr (member buffer blist))))
        (when (null upbuf)
          (setq upbuf (cadr *window-list*))
          (window-move upbuf 0 (window-x upbuf)))
        (window-resize upbuf
          (+ (window-nlines upbuf)
            (window-nlines buffer))
          (window-ncols upbuf))))
    (setq *window-list* (delete buffer *window-list*))
    t)))

(defun window-adjust-all ()
  (dolist (b *window-list*)
    (window-resize b
      (window-nlines b)
      cl-ncurses:*cols*))
  (dolist (b *window-list*)
    (when (<= cl-ncurses:*lines* (+ 2 (window-y b)))
      (window-delete b nil)))
  (let ((b (car (last *window-list*))))
    (window-resize b
      (+ (window-nlines b)
        (- cl-ncurses:*lines* *current-lines*))
      (window-ncols b)))
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (window-update-all))
