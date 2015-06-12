(in-package :lem)

(defun window-init ()
  (setq *current-buffer*
        (make-buffer (make-textbuf "main" nil)
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0)))

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
