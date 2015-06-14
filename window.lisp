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
  (let ((window
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
    window))

(defvar *window-list* nil)

(defvar *current-cols*)
(defvar *current-lines*)

(defun one-window-p ()
  (null (cdr *window-list*)))

(defun window-init ()
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (setq *current-window*
        (make-window (make-textbuf "main" nil)
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0))
  (setq *window-list* (list *current-window*)))

(defun window-offset-view (window)
  (let ((vtop-linum (window-vtop-linum window))
	(nlines (window-nlines window))
	(linum (window-cur-linum window)))
    (cond
      ((< #1=(+ vtop-linum nlines -2) linum)
	(- linum #1#))
      ((> vtop-linum linum)
	(- linum vtop-linum))
      (t
	0))))

(defun window-recenter (window)
  (setf (window-vtop-linum window)
        (window-cur-linum window))
  (window-scroll window (floor (window-nlines window) 2)))

(defun window-scroll (window n)
  (incf (window-vtop-linum window) n)
  (multiple-value-bind (outp offset)
      (buffer-head-line-p window (1+ (window-vtop-linum window)))
    (when outp
      (incf (window-vtop-linum window))))
  (multiple-value-bind (outp offset)
      (buffer-tail-line-p window (window-vtop-linum window))
    (when outp
      (incf (window-vtop-linum window)))))

(defun window-adjust-view (window recenter)
  (let ((offset (window-offset-view window)))
    (unless (zerop offset)
      (if recenter
        (window-recenter window)
	(window-scroll window offset)))))

(defun window-percentage (window)
  (cond
   ((<= (textbuf-nlines (window-textbuf window))
        (window-nlines window))
    "All")
   ((= 1 (window-vtop-linum window))
    "Top")
   ((<= (textbuf-nlines (window-textbuf window))
      (+ (window-vtop-linum window) (window-nlines window))
        )
    "Bot")
   (t
    (format nil "~2d%"
      (floor
       (* 100
	(float
	 (/ (window-vtop-linum window)
            (textbuf-nlines (window-textbuf window))))))))))

(defun window-redraw-modeline-1 (window bg-char)
  (cl-ncurses:mvwaddstr
   (window-win window)
   (1- (window-nlines window))
   0
   (let ((str (format nil "~c~c ~a: ~a (~{~a ~}) "
		bg-char ; read-only-flag
		(if (textbuf-modif-p (window-textbuf window)) #\* bg-char)
		"Lem"
		(textbuf-name (window-textbuf window))
		nil)))
     (format nil
       (format nil
	 "~~~d,,,'~ca ~a ~a"
	 (- (window-ncols window) 7)
	 bg-char
	 (window-percentage window)
	 (make-string 2 :initial-element bg-char))
       str))))

(defun window-redraw-modeline (window)
  (cl-ncurses:wattron (window-win window) cl-ncurses:a_reverse)
  (window-redraw-modeline-1 window #\-)
  (cl-ncurses:wattroff (window-win window) cl-ncurses:a_reverse))

(defun window-redraw-line (window str y)
  (let ((width (str-width str))
        (cury (- (window-cur-linum window)
                 (window-vtop-linum window)))
        (cols (window-ncols window))
        (curx))
    (when (= cury y)
      (setq curx (str-width str (window-cur-col window))))
    (cond
      ((< width (window-ncols window))
        nil)
      ((or (/= cury y)
           (< curx (1- cols)))
	(let ((i (wide-index str (1- cols))))
          (setq str
                (if (<= cols (str-width str i))
                  (format nil "~a $" (subseq str 0 (1- i)))
                  (format nil "~a$" (subseq str 0 i))))))
      ((< (window-cur-col window) (length str))
        (let* ((begin (wide-index str (- curx cols -3)))
	       (end (1+ (window-cur-col window)))
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
    (cl-ncurses:mvwaddstr (window-win window) y 0 str)
    curx))

(defun window-redraw-lines (window)
  (let (x)
    (do ((lines
           (textbuf-take-lines (window-textbuf window)
                               (window-vtop-linum window)
                               (1- (window-nlines window)))
           (cdr lines))
         (y 0 (1+ y)))
        ((null lines))
      (let ((curx (window-redraw-line window (car lines) y)))
        (when curx
          (setq x curx))))
    (cl-ncurses:wmove (window-win window)
                      (- (window-cur-linum window) (window-vtop-linum window))
                      x)))

(defun window-redraw (window)
  (cl-ncurses:werase (window-win window))
  (window-redraw-modeline window)
  (window-redraw-lines window)
  (cl-ncurses:wrefresh (window-win window)))

(defun window-update (window)
  (window-adjust-view window nil)
  (window-redraw window))

(defun window-update-all ()
  (dolist (win *window-list*)
    (unless (eq win *current-window*)
      (window-update win)))
  (window-update *current-window*))

(add-command 'window-vsplit 'split-window-vertical "C-x2")
(defun window-vsplit (window arg)
  (declare (ignore arg))
  (multiple-value-bind (nlines rem)
      (floor (window-nlines window) 2)
    (let ((newwin (make-window
                   (window-textbuf window)
                   nlines
                   (window-ncols window)
                   (+ (window-y window)
                     nlines
                     rem)
                   (window-x window))))
      (decf (window-nlines window) nlines)
      (cl-ncurses:wresize
       (window-win window)
       (window-nlines window)
       (window-ncols window))
      (setf (window-vtop-linum newwin)
            (window-vtop-linum window))
      (setf (window-cur-linum newwin)
            (window-cur-linum window))
      (setf (window-cur-col newwin)
            (window-cur-col window))
      (setf (window-max-col newwin)
            (window-max-col window))
      (setq *window-list*
        (sort (copy-list (append *window-list* (list newwin)))
          (lambda (b1 b2)
            (< (window-y b1) (window-y b2)))))))
  t)

(defun get-next-window (window)
  (let ((result (member window *window-list*)))
    (if (cdr result)
      (cadr result)
      (car *window-list*))))

(add-command 'window-next 'other-window "C-xo")
(defun window-next (window arg)
  (arg-repeat (arg t)
    (setq window (get-next-window window)))
  (setq *current-window* window)
  t)

(defun window-move (window y x)
  (cl-ncurses:mvwin (window-win window) y x)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-resize (window nlines ncols)
  (cl-ncurses:wresize (window-win window) nlines ncols)
  (setf (window-nlines window) nlines)
  (setf (window-ncols window) ncols))

(add-command 'window-delete-other-windows 'delete-other-windows "C-x1")
(defun window-delete-other-windows (window arg)
  (declare (ignore arg))
  (dolist (win *window-list*)
    (unless (eq win window)
      (cl-ncurses:delwin (window-win win))))
  (setq *window-list* (list window))
  (window-move window 0 0)
  (window-resize window
    (1- cl-ncurses:*lines*)
    cl-ncurses:*cols*)
  t)

(add-command 'window-delete 'delete-window "C-x0")
(defun window-delete (window arg)
  (declare (ignore arg))
  (cond
   ((one-window-p)
    (mb-write "Can not delete this window")
    nil)
   (t
    (when (eq *current-window* window)
      (window-next window nil))
    (cl-ncurses:delwin (window-win window))
    (let ((blist (reverse *window-list*)))
      (let ((upwin (cadr (member window blist))))
        (when (null upwin)
          (setq upwin (cadr *window-list*))
          (window-move upwin 0 (window-x upwin)))
        (window-resize upwin
          (+ (window-nlines upwin)
            (window-nlines window))
          (window-ncols upwin))))
    (setq *window-list* (delete window *window-list*))
    t)))

(defun window-adjust-all ()
  (dolist (win *window-list*)
    (window-resize win
      (window-nlines win)
      cl-ncurses:*cols*))
  (dolist (win *window-list*)
    (when (<= cl-ncurses:*lines* (+ 2 (window-y win)))
      (window-delete win nil)))
  (let ((win (car (last *window-list*))))
    (window-resize win
      (+ (window-nlines win)
        (- cl-ncurses:*lines* *current-lines*))
      (window-ncols win)))
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (window-update-all))
