(in-package :lem)

(defvar *mb-win*)

(defun mb-init ()
  (setq *mb-win*
	(cl-ncurses:newwin
	 1
	 cl-ncurses:*cols*
	 (1- cl-ncurses:*lines*)
	 0)))

(defun mb-clear ()
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:wrefresh *mb-win*))

(defun mb-write (msg)
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:mvwaddstr *mb-win* 0 0 msg)
  (cl-ncurses:wrefresh *mb-win*))

(defun mb-y-or-n-p (prompt)
  (do () (nil)
    (cl-ncurses:werase *mb-win*)
    (cl-ncurses:mvwaddstr *mb-win* 0 0 (format nil "~a [y/n]?" prompt))
    (cl-ncurses:wrefresh *mb-win*)
    (let ((c (getch)))
      (cond
       ((char= #\y c)
        (return t))
       ((char= #\n c)
        (return nil))))))

(defun mb-read-char (prompt)
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:mvwaddstr *mb-win* 0 0 prompt)
  (cl-ncurses:wrefresh *mb-win*)
  (getch))

(defun mb-readline (prompt)
  (let ((str ""))
    (do ((break nil))
        (break)
      (cl-ncurses:werase *mb-win*)
      (cl-ncurses:mvwaddstr *mb-win* 0 0 (format nil "~a~a" prompt str))
      (cl-ncurses:wrefresh *mb-win*)
      (let ((c (getch)))
        (cond
	  ((char= c key::ctrl-j)
	   (setq break t))
	  ((char= c key::ctrl-h)
	   (setq str (subseq str 0 (1- (length str)))))
	  (t
	   (setq str (concatenate 'string str (string c)))))))
    str))
