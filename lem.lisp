(in-package :lem)

(defvar *exit*)
(defvar *universal-argument* nil)

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

(defun ungetch (c)
  (cl-ncurses:ungetch (char-code c)))

(define-key *global-keymap* "C-xC-c" 'exit-lem)
(defcommand exit-lem () ()
  (when (or (not (any-modified-buffer-p))
          (y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(define-key *global-keymap* "C-u" 'universal-argument)
(defcommand universal-argument () ()
  (let ((numlist)
        n)
    (do ((c (read-char "C-u 4")
            (read-char
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
        (ungetch c)
        (setq *universal-argument*
          (if numlist
            (parse-integer (format nil "~{~a~}" numlist))
            4))
        (return (main-step)))))))

(defun input-keys ()
  (let ((c (getch)))
    (if (or (char= c key::ctrl-x)
          (char= c key::escape))
      (list c (getch))
      (let ((bytes (utf8-bytes (char-code c))))
        (if (= bytes 1)
          (list c)
          (let ((bytes (coerce
                        (mapcar 'char-code
                          (cons c
                            (loop repeat (1- bytes)
                              collect (getch))))
                        '(vector (unsigned-byte 8)))))
            (list (aref (bytes-to-string bytes) 0))))))))


(defun execute (keys)
  (let* ((keymap *current-keymap*)
         (cmd (keymap-find-command keymap keys)))
    (if cmd
      (cmd-call cmd *universal-argument*)
      (key-undef-hook keymap keys))))

(defun main-step ()
  (let ((keys (input-keys)))
    (mb-clear)
    (execute keys)
    (setq *universal-argument* nil)))

(defun undefined-key (keys)
  (let ((c (insertion-key-p keys)))
    (if c
      (insert-char c
        (or *universal-argument* 1))
      (mb-write "Key not found"))))

(defun lem-init (args)
  (cl-ncurses:initscr)
  (cl-ncurses:noecho)
  (cl-ncurses:cbreak)
  (cl-ncurses:raw)
  (cl-ncurses:refresh)
  (window-init)
  (mb-init)
  (dolist (arg args)
    (file-open arg)))

(defun lem-finallize ()
  (cl-ncurses:endwin))

(defun lem-main ()
  (do ((*exit* nil)
       (*curr-kill-flag* nil nil)
       (*last-kill-flag* nil *curr-kill-flag*))
      (*exit*)
    (window-update-all)
    (when (catch 'abort
            (main-step)
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
