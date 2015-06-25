(in-package :lem)

(defvar *exit*)

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(let ((queue (make-tlist)))
  (defun getch (&optional (abort-jump t))
    (let* ((code (if (not (tlist-empty-p queue))
                   (tlist-rem-left queue)
                   (cl-ncurses:wgetch (window-win))))
           (char (code-char code)))
      (when *macro-recording-p*
        (push char *macro-chars*))
      (cond
       ((= code 410)
        (mb-resize)
        (adjust-screen-size)
        (getch))
       ((and (char= char key::ctrl-g) abort-jump)
        (throw 'abort t))
       (t char))))
  (defun ungetch (c)
    (tlist-add-right queue (char-code c)))
  (defun getch-queue-length ()
    (length (car queue))))

(define-key *global-keymap* "C-g" 'keyboard-quit)
(defcommand keyboard-quit () ()
  (setq *universal-argument* nil)
  (setq *macro-recording-p* nil)
  (write-message "Quit"))

(define-key *global-keymap* "C-xC-c" 'exit-lem)
(defcommand exit-lem () ()
  (when (or (not (any-modified-buffer-p))
          (y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(define-key *global-keymap* "C-x?" 'describe-key)
(defcommand describe-key () ()
  (write-message "describe-key: ")
  (let* ((keys (input-keys))
         (cmd (keymap-find-command (current-mode-keymap) keys)))
    (write-message (format nil "describe-key: ~a ~a"
                     (keys-to-keystr keys)
                     cmd))))

(define-key *global-keymap* "C-x(" 'begin-macro)
(defcommand begin-macro () ()
  (write-message "Start macro")
  (setq *macro-recording-p* t)
  (setq *macro-chars* nil))

(define-key *global-keymap* "C-x)" 'end-macro)
(defcommand end-macro () ()
  (when *macro-recording-p*
    (setq *macro-recording-p* nil)
    (setq *macro-chars* (nreverse *macro-chars*))
    (write-message "End macro"))
  t)

(define-key *global-keymap* "C-xe" 'execute-macro)
(defcommand execute-macro (n) ("p")
  (let ((*macro-running-p* t)
        (*universal-argument* nil))
    (loop repeat n while *macro-running-p* do
      (let ((length (getch-queue-length)))
        (dolist (c *macro-chars*)
          (ungetch c))
        (do ()
            ((or (not *macro-running-p*)
                 (>= length (getch-queue-length))))
            (main-step))))))

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
  (let ((c (getch nil)))
    (if (or (char= c key::ctrl-x)
          (char= c key::escape))
      (list c (getch nil))
      (let ((bytes (utf8-bytes (char-code c))))
        (if (= bytes 1)
          (list c)
          (let ((bytes (coerce
                        (mapcar 'char-code
                          (cons c
                            (loop repeat (1- bytes)
                              collect (getch nil))))
                        '(vector (unsigned-byte 8)))))
            (list (aref (bytes-to-string bytes) 0))))))))

(defun execute (keys)
  (let* ((keymap (current-mode-keymap))
         (cmd (keymap-find-command keymap keys)))
    (if cmd
      (unless (cmd-call cmd *universal-argument*)
        (setq *macro-running-p* nil))
      (key-undef-hook keymap keys))))

(defun main-step ()
  (let ((keys (input-keys)))
    (clear-message-line)
    (execute keys)
    (setq *universal-argument* nil)))

(defun undefined-key (keys)
  (let ((c (insertion-key-p keys)))
    (if c
      (insert-char c
        (or *universal-argument* 1))
      (write-message "Key not found"))))

(defun lem-init (args)
  (cl-ncurses:initscr)
  (cl-ncurses:noecho)
  (cl-ncurses:cbreak)
  (cl-ncurses:raw)
  (cl-ncurses:refresh)
  (window-init)
  (mb-init)
  (add-hook 'find-file-hooks
            (lambda ()
              (lisp-mode)))
  (dolist (arg args)
    (find-file arg)))

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
      (write-message "Abort"))))

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
