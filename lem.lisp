(in-package :lem)

(defvar *exit*)

(defun getch ()
  (let* ((code (cl-ncurses:wgetch
                (window-win *current-window*)))
         (char (code-char code)))
    (cond
     ((= code 410)
      (mb-resize)
      (window-adjust-all)
      (getch))
     ((char= char key::ctrl-g)
      (throw 'abort t))
     (t char))))

(add-command 'exit-lem 'exit-lem "C-xC-c")
(defun exit-lem (buffer arg)
  (declare (ignore arg))
  (when (or (not (tblist-any-modif-p))
          (mb-y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(defun self-insert (c arg)
  (buffer-insert-char *current-window* c arg))

(defun execute (keys arg)
  (let ((cmd (find-command keys)))
    (cond
     (cmd
      (funcall cmd *current-window* arg))
     ((or (< 31 (char-code (car keys)))
        (char= key::ctrl-i (car keys)))
      (self-insert (car keys) arg))
     (t
      (mb-write "Key not found")))))

(defun universal-argument ()
  (let ((numlist)
        n)
    (do ((c (mb-read-char "C-u 4")
            (mb-read-char
             (format nil "C-u ~{~a~}" numlist))))
        (nil)
      (cond
       ((char= c key::ctrl-u)
        (setq numlist
          (mapcar 'digit-char-p
            (coerce
             (format nil "~a"
              (* 4
                (if numlist
                  (parse-integer
                   (format nil "~{~a~}" numlist))
                  4)))
             'list))))
       ((and (char= c #\-) (null numlist))
        (setq numlist (append numlist (list #\-))))
       ((setq n (digit-char-p c))
        (setq numlist
          (append numlist (list n))))
       (t
        (return
         (values
          c
          (if numlist
            (parse-integer (format nil "~{~a~}" numlist))
            4))))))))

(defun input-keys ()
  (let ((c (getch))
        uarg)
    (when (char= c key::ctrl-u)
      (multiple-value-setq (c uarg)
        (universal-argument)))
    (if (or (char= c key::ctrl-x)
            (char= c key::escape))
      (values (list c (getch)) uarg)
      (let ((bytes (utf8-bytes (char-code c))))
	(if (= bytes 1)
	  (values (list c) uarg)
          (let ((bytes (coerce
                        (mapcar 'char-code
                          (cons c
                            (loop repeat (1- bytes)
                              collect (getch))))
                        '(vector (unsigned-byte 8)))))
            (values
             (list (aref (sb-ext:octets-to-string bytes) 0))
             uarg)))))))

(defun lem-init (args)
  (cl-ncurses:initscr)
  (cl-ncurses:noecho)
  (cl-ncurses:cbreak)
  (cl-ncurses:raw)
  (cl-ncurses:refresh)
  (window-init)
  (mb-init)
  (dolist (arg args)
    (file-open *current-window* arg)))

(defun lem-finallize ()
  (cl-ncurses:endwin))

(defun lem-main ()
  (do ((*exit* nil)) (*exit*)
    (window-update-all)
    (when (catch 'abort
            (multiple-value-bind (keys uarg) (input-keys)
              (mb-clear)
              (execute keys uarg))
            nil)
      (mb-write "Abort"))))

(defun lem (&rest args)
  (let ((*print-circle* t))
    (with-open-file (*error-output* "ERROR"
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
      (unwind-protect
       (progn
        (lem-init args)
        (lem-main))
       (lem-finallize)))))
