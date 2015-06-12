(in-package :lem)

(defvar *mb-win*)
(defvar *mb-print-flag* nil)

(defun mb-init ()
  (setq *mb-win*
	(cl-ncurses:newwin
	 1
	 cl-ncurses:*cols*
	 (1- cl-ncurses:*lines*)
	 0)))

(defun mb-clear ()
  (when *mb-print-flag*
    (cl-ncurses:werase *mb-win*)
    (cl-ncurses:wrefresh *mb-win*)
    (setq *mb-print-flag* nil)))

(defun mb-write (msg)
  (setq *mb-print-flag* t)
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:mvwaddstr *mb-win* 0 0 msg)
  (cl-ncurses:wrefresh *mb-win*))

(defun mb-y-or-n-p (prompt)
  (setq *mb-print-flag* t)
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
  (setq *mb-print-flag* t)
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:mvwaddstr *mb-win* 0 0 prompt)
  (cl-ncurses:wrefresh *mb-win*)
  (getch))

(defun mb-readline (prompt)
  (setq *mb-print-flag* t)
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
