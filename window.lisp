(in-package :lem)

(defclass window ()
  ((win
    :initarg :win)
   (nlines
    :initarg :nlines)
   (ncols
    :initarg :ncols)
   (y
    :initarg :y)
   (x
    :initarg :x)
   (buffer
    :initarg :buffer)
   (vtop-linum
    :initarg :vtop-linum)
   (cur-linum
    :initarg :cur-linum)
   (cur-col
    :initarg :cur-col)
   (max-col
    :initarg :max-col)))

(defun window-win (&optional (window *current-window*))
  (slot-value window 'win))

(defun window-nlines (&optional (window *current-window*))
  (slot-value window 'nlines))

(defun window-ncols (&optional (window *current-window*))
  (slot-value window 'ncols))

(defun window-y (&optional (window *current-window*))
  (slot-value window 'y))

(defun window-x (&optional (window *current-window*))
  (slot-value window 'x))

(defun window-buffer (&optional (window *current-window*))
  (slot-value window 'buffer))

(defun window-vtop-linum (&optional (window *current-window*))
  (slot-value window 'vtop-linum))

(defun window-cur-linum (&optional (window *current-window*))
  (slot-value window 'cur-linum))

(defun window-cur-col (&optional (window *current-window*))
  (slot-value window 'cur-col))

(defun window-max-col(&optional (window *current-window*))
  (slot-value window 'max-col))

(defun (setf window-win) (val &optional (window *current-window*))
  (setf (slot-value window 'win) val)
  val)

(defun (setf window-nlines) (val &optional (window *current-window*))
  (setf (slot-value window 'nlines) val)
  val)

(defun (setf window-ncols) (val &optional (window *current-window*))
  (setf (slot-value window 'ncols) val)
  val)

(defun (setf window-y) (val &optional (window *current-window*))
  (setf (slot-value window 'y) val)
  val)

(defun (setf window-x) (val &optional (window *current-window*))
  (setf (slot-value window 'x) val)
  val)

(defun (setf window-buffer) (val &optional (window *current-window*))
  (setf (slot-value window 'buffer) val)
  val)

(defun (setf window-vtop-linum) (val &optional (window *current-window*))
  (setf (slot-value window 'vtop-linum) val)
  val)

(defun (setf window-cur-linum) (val &optional (window *current-window*))
  (setf (slot-value window 'cur-linum) val)
  val)

(defun (setf window-cur-col) (val &optional (window *current-window*))
  (setf (slot-value window 'cur-col) val)
  val)

(defun (setf window-max-col) (val &optional (window *current-window*))
  (setf (slot-value window 'max-col) val)
  val)

(defun make-window (buffer nlines ncols y x)
  (let ((window
         (make-instance 'window
          :win (cl-ncurses:newwin nlines ncols y x)
          :nlines nlines
          :ncols ncols
          :y y
          :x x
          :buffer buffer
          :vtop-linum 1
          :cur-linum 1
          :cur-col 0
          :max-col 0)))
    window))

(defvar *current-cols*)
(defvar *current-lines*)

(defun one-window-p ()
  (null (cdr *window-list*)))

(defun window-init ()
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (setq *current-window*
        (make-window (make-buffer "main" nil)
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0))
  (setq *prev-buffer* (window-buffer))
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
      (head-line-p window (1+ (window-vtop-linum window)))
    (when outp
      (incf (window-vtop-linum window))))
  (multiple-value-bind (outp offset)
      (tail-line-p window (window-vtop-linum window))
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
   ((<= (buffer-nlines (window-buffer window))
        (window-nlines window))
    "All")
   ((= 1 (window-vtop-linum window))
    "Top")
   ((<= (buffer-nlines (window-buffer window))
      (+ (window-vtop-linum window) (window-nlines window))
        )
    "Bot")
   (t
    (format nil "~2d%"
      (floor
       (* 100
	(float
	 (/ (window-vtop-linum window)
            (buffer-nlines (window-buffer window))))))))))

(defun window-redraw-modeline-1 (window bg-char)
  (cl-ncurses:mvwaddstr
   (window-win window)
   (1- (window-nlines window))
   0
   (let ((str (format nil "~c~c ~a: ~a (~{~a ~}) "
		bg-char ; read-only-flag
		(if (buffer-modified-p (window-buffer window)) #\* bg-char)
		"Lem"
		(buffer-name (window-buffer window))
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
           (buffer-take-lines (window-buffer window)
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

(define-key "C-x2" 'split-window)
(defcommand split-window () ()
  (multiple-value-bind (nlines rem)
      (floor (window-nlines) 2)
    (let ((newwin (make-window
                   (window-buffer)
                   nlines
                   (window-ncols)
                   (+ (window-y)
                     nlines
                     rem)
                   (window-x))))
      (decf (window-nlines) nlines)
      (cl-ncurses:wresize
       (window-win)
       (window-nlines)
       (window-ncols))
      (setf (window-vtop-linum newwin)
            (window-vtop-linum))
      (setf (window-cur-linum newwin)
            (window-cur-linum))
      (setf (window-cur-col newwin)
            (window-cur-col))
      (setf (window-max-col newwin)
            (window-max-col))
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

(define-key "C-xo" 'other-window)
(defcommand other-window (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (setq *current-window*
      (get-next-window *current-window*))))

(defun window-move (window y x)
  (cl-ncurses:mvwin (window-win window) y x)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-resize (window nlines ncols)
  (cl-ncurses:wresize (window-win window) nlines ncols)
  (setf (window-nlines window) nlines)
  (setf (window-ncols window) ncols))

(define-key "C-x1" 'delete-other-windows)
(defcommand delete-other-windows () ()
  (dolist (win *window-list*)
    (unless (eq win *current-window*)
      (cl-ncurses:delwin (window-win win))))
  (setq *window-list* (list *current-window*))
  (window-move *current-window* 0 0)
  (window-resize *current-window*
    (1- cl-ncurses:*lines*)
    cl-ncurses:*cols*)
  t)

(define-key "C-x0" 'delete-window)
(defcommand delete-window () ()
  (delete-window-1 *current-window*))

(defun delete-window-1 (window)
  (cond
   ((one-window-p)
    (mb-write "Can not delete this window")
    nil)
   (t
    (when (eq *current-window* window)
      (other-window))
    (cl-ncurses:delwin (window-win window))
    (let ((wlist (reverse *window-list*)))
      (let ((upwin (cadr (member window wlist))))
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
      (delete-window-1 win)))
  (let ((win (car (last *window-list*))))
    (window-resize win
      (+ (window-nlines win)
        (- cl-ncurses:*lines* *current-lines*))
      (window-ncols win)))
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (window-update-all))

(defun pop-to-buffer (buffer)
  (let ((old-window *current-window*))
    (when (one-window-p)
      (split-window))
    (other-window)
    (set-buffer buffer)
    (setq *current-window* old-window)))
