(in-package :lem)

(export '(minibuf-clear
          minibuf-print
          minibuf-y-or-n-p
          minibuf-read-char
          minibuf-read-line
          minibuf-read-string
          minibuf-read-number
          minibuf-read-buffer
          minibuf-read-file))

(defvar *mb-win*)
(defvar *mb-print-flag* nil)
(defvar *mb-read-log* nil)

(defun minibuf-init ()
  (setq *mb-win*
        (cl-charms/low-level:newwin
         1
         cl-charms/low-level:*cols*
         (1- cl-charms/low-level:*lines*)
         0)))

(defun minibuf-resize ()
  (cl-charms/low-level:mvwin *mb-win*
                             (1- cl-charms/low-level:*lines*)
                             0)
  (cl-charms/low-level:wresize *mb-win*
                               1
                               cl-charms/low-level:*cols*)
  (cl-charms/low-level:werase *mb-win*)
  (cl-charms/low-level:wrefresh *mb-win*))

(defun minibuf-clear ()
  (when *mb-print-flag*
    (cl-charms/low-level:werase *mb-win*)
    (cl-charms/low-level:wrefresh *mb-win*)
    (setq *mb-print-flag* nil)))

(defun minibuf-print (msg)
  (setq *mb-print-flag* t)
  (cl-charms/low-level:werase *mb-win*)
  (cl-charms/low-level:mvwaddstr
   *mb-win*
   0
   0
   (replace-string (string #\newline) "<NL>" msg))
  (cl-charms/low-level:wrefresh *mb-win*))

(defun minibuf-y-or-n-p (prompt)
  (setq *mb-print-flag* t)
  (do () (nil)
    (minibuf-print (format nil "~a [y/n]? " prompt))
    (let ((c (getch)))
      (cond
       ((char= #\y c)
        (return t))
       ((char= #\n c)
        (return nil))))))

(defun minibuf-read-char (prompt)
  (setq *mb-print-flag* t)
  (minibuf-print prompt)
  (getch))

(defun minibuf-read-line (prompt initial comp-f existing-p)
  (setq *mb-print-flag* t)
  (let ((str initial)
        (comp-flag)
        (one-window-p (one-window-p))
        (prev-read-log *mb-read-log*)
        (next-read-log))
    (do ((break nil))
        (break)
      (minibuf-print (format nil "~a~a" prompt str))
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
         ((or (char= c key::ctrl-h)
              (char= c key::backspace)
              (char= c #\rubout))
          (when (< 0 (length str))
            (setq str (subseq str 0 (1- (length str))))))
         ((char= c key::ctrl-u)
          (setq str ""))
         ((char= c key::ctrl-p)
          (when prev-read-log
            (let ((elt (pop prev-read-log)))
              (push elt next-read-log)
              (setq str elt))))
         ((char= c key::ctrl-n)
          (when next-read-log
            (let ((elt (pop next-read-log)))
              (push elt prev-read-log)
              (setq str elt))))
         ((char= c key::ctrl-q)
          (setq str (concatenate 'string str (string (getch)))))
         ((char= c key::ctrl-y)
          (let ((str1 (kill-ring-first)))
            (when str1
              (setq str (concatenate 'string str str1)))))
         (t
          (let ((c (input-char (char-code c))))
            (setq str (concatenate 'string str (string c))))))))
    (cond
     ((and comp-flag one-window-p)
      (delete-completion-window))
     ((and comp-flag *completion-window*)
      (let ((*current-window* *completion-window*))
        (set-buffer (car *buffer-list*)))))
    (push str *mb-read-log*)
    str))

(defun minibuf-read-string (prompt &optional initial)
  (minibuf-read-line prompt (or initial "") nil nil))

(defun minibuf-read-number (prompt &optional min max)
  (parse-integer
   (minibuf-read-line prompt "" nil
                      #'(lambda (str)
                          (multiple-value-bind (n len)
                              (parse-integer str :junk-allowed t)
                            (and
                             n
                             (/= 0 (length str))
                             (= (length str) len)
                             (if min (<= min n) t)
                             (if max (<= n max) t)))))))

(defun minibuf-read-buffer (prompt &optional default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let* ((buffer-names (mapcar 'buffer-name *buffer-list*))
         (result (minibuf-read-line
                  prompt
                  ""
                  #'(lambda (name)
                      (completion name buffer-names))
                  (and existing
                       #'(lambda (name)
                           (member name buffer-names :test 'string=))))))
    (if (string= result "")
        default
        result)))

(defun minibuf-read-file (prompt &optional directory default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let ((result (minibuf-read-line prompt
                                   directory
                                   #'file-completion
                                   (and existing #'file-exist-p))))
    (if (string= result "")
        default
        result)))
