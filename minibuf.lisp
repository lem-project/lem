(in-package :lem)

(defvar *comp-buffer-name* "*Completion*")
(defvar *completion-window* nil)

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

(defun mb-clear ()
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

(defun mb-completion (comp-f str)
  (multiple-value-bind (result strings) (funcall comp-f str)
    (let ((buffer (get-buffer-create *comp-buffer-name*)))
      (buffer-erase buffer)
      (dolist (s strings)
        (buffer-append-line buffer s))
      (setq *completion-window* (pop-to-buffer buffer))
      (window-update-all))
    (or result str)))

(defun mb-readline (prompt initial comp-f existing-p)
  (setq *mb-print-flag* t)
  (let ((str initial)
        (comp-flag))
    (do ((break nil))
        (break)
      (write-message (format nil "~a~a" prompt str))
      (let ((c (getch)))
        (cond
         ((char= c key::ctrl-j)
          (when (or (string= str "")
                  (null existing-p)
                  (funcall existing-p str))
            (setq break t)))
         ((char= c key::ctrl-i)
          (when comp-f
            (setq comp-flag t)
            (setq str
              (mb-completion comp-f str))))
         ((char= c key::ctrl-h)
          (setq str (subseq str 0 (1- (length str)))))
         ((char= c key::ctrl-q)
          (setq str (concatenate 'string str (string (getch)))))
         (t
          (setq str (concatenate 'string str (string c)))))))
    (when comp-flag
      (let ((*current-window* *completion-window*))
        (delete-window)))
    str))

(defun read-string (prompt &optional initial)
  (mb-readline prompt (or initial "") nil nil))

(defun read-number (prompt)
  (parse-integer
   (mb-readline prompt "" nil
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
         (result (mb-readline prompt
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
  (let ((result (mb-readline prompt
                  directory
                  #'file-completion
                  (and existing #'file-exist-p))))
    (if (string= result "")
      default
      result)))
