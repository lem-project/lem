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

(defmethod window-redraw ((buffer buffer))
  (cl-ncurses:werase (buffer-win buffer))
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
                      x))
  (cl-ncurses:wrefresh (buffer-win buffer)))

(defmethod window-update ((buffer buffer))
  (window-adjust-view buffer nil)
  (window-redraw buffer))
