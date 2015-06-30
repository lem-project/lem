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

(defun mb-resize ()
  (cl-ncurses:mvwin *mb-win*
    (1- cl-ncurses:*lines*)
    0)
  (cl-ncurses:wresize *mb-win*
    1
    cl-ncurses:*cols*)
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:wrefresh *mb-win*))

(defun clear-message-line ()
  (when *mb-print-flag*
    (cl-ncurses:werase *mb-win*)
    (cl-ncurses:wrefresh *mb-win*)
    (setq *mb-print-flag* nil)))

(defun write-message (msg)
  (setq *mb-print-flag* t)
  (cl-ncurses:werase *mb-win*)
  (cl-ncurses:mvwaddstr *mb-win*
    0
    0
    (replace-string (string #\newline) "<NL>" msg))
  (cl-ncurses:wrefresh *mb-win*))

(defun y-or-n-p (prompt)
  (setq *mb-print-flag* t)
  (do () (nil)
    (write-message (format nil "~a [y/n]? " prompt))
    (let ((c (getch)))
      (cond
       ((char= #\y c)
        (return t))
       ((char= #\n c)
        (return nil))))))

(defun read-char (prompt)
  (setq *mb-print-flag* t)
  (write-message prompt)
  (getch))

(defun read-minibuffer (prompt initial comp-f existing-p)
  (setq *mb-print-flag* t)
  (let ((str initial)
        (comp-flag)
        (one-window-p (one-window-p)))
    (do ((break nil))
        (break)
      (write-message (format nil "~a~a" prompt str))
      (let ((c (getch)))
        (cond
         ((or (char= c key::ctrl-j)
              (char= c key::ctrl-m))
          (when (or (string= str "")
                  (null existing-p)
                  (funcall existing-p str))
            (setq break t)))
         ((char= c key::ctrl-i)
          (when comp-f
            (setq comp-flag t)
            (setq str
              (popup-completion comp-f str))))
         ((char= c key::ctrl-h)
          (when (< 0 (length str))
            (setq str (subseq str 0 (1- (length str))))))
         ((char= c key::ctrl-u)
          (setq str ""))
         ((char= c key::ctrl-q)
          (setq str (concatenate 'string str (string (getch)))))
         (t
          (setq str (concatenate 'string str (string c)))))))
    (cond
     ((and comp-flag one-window-p)
      (delete-completion-window))
     ((and comp-flag *completion-window*)
      (let ((*current-window* *completion-window*))
        (set-buffer *prev-buffer*))))
    str))

(defun read-string (prompt &optional initial)
  (read-minibuffer prompt (or initial "") nil nil))

(defun read-number (prompt)
  (parse-integer
   (read-minibuffer prompt "" nil
     (lambda (str)
       (multiple-value-bind (n len)
           (parse-integer str :junk-allowed t)
         (and
          n
          (/= 0 (length str))
          (= (length str) len)))))))

(defun read-buffer (prompt &optional default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let* ((buffer-names (mapcar 'buffer-name *buffer-list*))
         (result (read-minibuffer prompt
                   ""
                   (lambda (name)
                     (completion name buffer-names))
                   (and existing
                     (lambda (name)
                       (member name buffer-names :test 'string=))))))
    (if (string= result "")
      default
      result)))

(defun read-file-name (prompt &optional directory default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let ((result (read-minibuffer prompt
                  directory
                  #'file-completion
                  (and existing #'file-exist-p))))
    (if (string= result "")
      default
      result)))
