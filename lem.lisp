(in-package :lem)

(defvar *exit*)

(defgeneric exit-lem (buffer arg))
(defmethod exit-lem ((buffer buffer) arg)
  (declare (ignore arg))
  (when (or (not (tblist-any-modif-p))
          (mb-y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(defun self-insert (c arg)
  (arg-repeat (arg t)
    (buffer-insert-char *current-buffer* c arg)))

(defun execute (keys arg)
  (let ((cmd (command-find-keybind keys)))
    (cond
     (cmd
      (funcall cmd *current-buffer* arg))
     ((or (< 31 (car keys))
        (= key::ctrl-i (car keys)))
      (self-insert (code-char (car keys)) arg))
     (t
      (mb-write "Key not found")))))

(defun input-keys ()
  (let ((c (cl-ncurses:getch)))
    (if (or (= c key::ctrl-x)
            (= c key::escape))
      (list c (cl-ncurses:getch))
      (let ((bytes (utf8-bytes c)))
	(if (= bytes 1)
	  (list c)
          (let ((bytes
                  (coerce
                    (cons c
                          (loop repeat (1- bytes)
                                collect (cl-ncurses:getch)))
                  '(vector (unsigned-byte 8)))))
            (list (char-code (aref (sb-ext:octets-to-string bytes) 0)))))))))

(defun lem-init (args)
  (cl-ncurses:initscr)
  (cl-ncurses:noecho)
  (cl-ncurses:cbreak)
  (cl-ncurses:raw)
  (cl-ncurses:refresh)
  (command-init)
  (window-init)
  (mb-init)
  (dolist (arg args)
    (file-open *current-buffer* arg)))

(defun lem-finallize ()
  (cl-ncurses:endwin))

(defun lem-main ()
  (do ((*exit* nil)) (*exit*)
    (window-update *current-buffer*)
    (let ((keys (input-keys)))
      (mb-clear)
      (execute keys nil))))

(defun lem (&rest args)
  (with-open-file (*error-output* "ERROR"
		   :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (unwind-protect
	(progn
	  (lem-init args)
          (lem-main))
      (lem-finallize))))
