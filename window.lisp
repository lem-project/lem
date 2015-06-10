(in-package :lem)

(defgeneric window-offset-view (buffer))
(defgeneric window-recenter (buffer))
(defgeneric window-scroll (buffer n))
(defgeneric window-adjust-view (buffer recenter))
(defgeneric window-redraw (buffer))
(defgeneric window-update (buffer))

(defun window-init ()
  (setq *current-buffer*
        (make-buffer (make-textbuf "main")
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0)))

(defmethod window-offset-view ((buffer buffer))
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

(defmethod window-recenter ((buffer buffer))
  (setf (buffer-vtop-linum buffer)
        (buffer-cur-linum buffer))
  (window-scroll buffer (floor (buffer-nlines buffer) 2)))

(defmethod window-scroll ((buffer buffer) n)
  (incf (buffer-vtop-linum buffer) n)
  (multiple-value-bind (outp offset)
      (buffer-head-line-p buffer (1+ (buffer-vtop-linum buffer)))
    (when outp
      (incf (buffer-vtop-linum buffer))))
  (multiple-value-bind (outp offset)
      (buffer-tail-line-p buffer (buffer-vtop-linum buffer))
    (when outp
      (incf (buffer-vtop-linum buffer)))))

(defmethod window-adjust-view ((buffer buffer) recenter)
  (let ((offset (window-offset-view buffer)))
    (unless (zerop offset)
      (if recenter
        (window-recenter buffer)
	(window-scroll buffer offset)))))

(defun window-redraw-line (buffer str y)
  (let ((width (str-width str))
	(cury (- (buffer-cur-linum buffer)
		 (buffer-vtop-linum buffer)))
	(cols (buffer-ncols buffer))
	(curx (buffer-cur-col buffer)))
    (cond
      ((< width (buffer-ncols buffer))
	nil)
      ((or (/= cury y)
	   (< curx (1- cols)))
	(setq str
	      (concatenate 'string
                           (substring-width str 0 (1- cols))
                           (string #\$))))
      (t
	)))
  (cl-ncurses:mvwaddstr (buffer-win buffer) y 0 str))

(defmethod window-redraw ((buffer buffer))
  (cl-ncurses:werase (buffer-win buffer))
  (do ((lines
         (textbuf-take-lines (buffer-textbuf buffer)
                             (buffer-vtop-linum buffer)
                             (1- (buffer-nlines buffer)))
         (cdr lines))
       (y 0 (1+ y)))
      ((null lines))
    (window-redraw-line buffer (car lines) y))
  (cl-ncurses:wmove (buffer-win buffer)
		    (- (buffer-cur-linum buffer) (buffer-vtop-linum buffer))
		    (buffer-cur-col buffer))
  (cl-ncurses:wrefresh (buffer-win buffer)))

(defmethod window-update ((buffer buffer))
  (window-adjust-view buffer nil)
  (window-redraw buffer))
