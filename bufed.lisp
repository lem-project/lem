(in-package :lem)

(define-key *global-keymap* "M-~" 'unmark-buffer)
(defcommand unmark-buffer () ()
  (setf (buffer-modified-p (window-buffer)) nil)
  t)

(define-key *global-keymap* "C-xC-q" 'toggle-read-only)
(defcommand toggle-read-only () ()
  (setf (buffer-read-only-p (window-buffer))
    (not (buffer-read-only-p (window-buffer))))
  t)

(defun set-buffer (buffer)
  (unless (eq (window-buffer) buffer)
    (let ((old-buf (window-buffer)))
      (setq *prev-buffer* old-buf)
      (setf (buffer-keep-binfo old-buf)
            (list (window-vtop-linum)
                  (window-cur-linum)
                  (window-cur-col)
                  (window-max-col))))
    (setf (window-buffer) buffer)
    (let ((vtop-linum 1)
          (cur-linum 1)
          (cur-col 0)
          (max-col 0))
      (when (buffer-keep-binfo buffer)
        (multiple-value-setq
         (vtop-linum cur-linum cur-col max-col)
         (apply 'values (buffer-keep-binfo buffer))))
      (setf (window-vtop-linum) vtop-linum)
      (setf (window-cur-linum) cur-linum)
      (setf (window-cur-col) cur-col)
      (setf (window-max-col) max-col))))

(defun head-line-p (window linum)
  (declare (ignore window))
  (values (<= linum 1) (- 2 linum)))

(defun tail-line-p (window linum)
  (let ((nlines (buffer-nlines (window-buffer window))))
    (values (<= nlines linum) (- nlines linum))))

(defun bolp ()
  (zerop (window-cur-col)))

(defun eolp ()
  (= (window-cur-col)
     (buffer-line-length
      (window-buffer)
      (window-cur-linum))))

(defun bobp ()
  (and (head-line-p *current-window* (window-cur-linum))
       (bolp)))

(defun eobp ()
  (and (tail-line-p
        *current-window*
        (window-cur-linum))
       (eolp)))

(defun insert-char (c n)
  (dotimes (_ n t)
    (when (buffer-insert-char
           (window-buffer)
           (window-cur-linum)
           (window-cur-col)
           c)
      (next-char 1))))

(defun insert-lines (lines)
  (do ((rest lines (cdr rest)))
      ((null rest))
    (buffer-insert-line
     (window-buffer)
     (window-cur-linum)
     (window-cur-col)
     (car rest))
    (next-char (length (car rest)))
    (when (cdr rest)
      (insert-newline 1))))

(defcommand insert-string (str) ("sInsert string: ")
  (insert-lines (split-string str #\newline)))

(define-key *global-keymap* "C-j" 'insert-newline)
(defcommand insert-newline (n) ("p")
  (dotimes (_ n t)
    (buffer-insert-newline (window-buffer)
                            (window-cur-linum)
                            (window-cur-col))
    (next-line 1)))

(define-key *global-keymap* "C-o" 'open-line)
(defcommand open-line (n) ("p")
  (insert-newline n)
  (prev-char n))

(define-key *global-keymap* "C-d" 'delete-char)
(defcommand delete-char (&optional n) ("P")
  (cond
   ((and n (minusp n))
    (backward-delete-char (- n)))
   (t
    (multiple-value-bind (result lines)
        (buffer-delete-char
         (window-buffer)
         (window-cur-linum)
         (window-cur-col)
         (or n 1))
      (when result
        (when n
          (with-kill ()
            (kill-push lines)))
        (dolist (win *window-list*)
          (when (and
                 (not (eq win *current-window*))
                 (eq (window-buffer win) (window-buffer))
                 (> (window-cur-linum win)
                   (buffer-nlines (window-buffer win))))
            (decf (window-cur-linum win)
              (- (window-cur-linum win)
                (buffer-nlines (window-buffer win))))))
        result)))))

(define-key *global-keymap* "C-h" 'backward-delete-char)
(defcommand backward-delete-char (n) ("p")
  (if (minusp n)
    (delete-char (- n))
    (when (prev-char n)
      (delete-char n))))

(define-key *global-keymap* "C-k" 'kill-line)
(defcommand kill-line (&optional n) ("P")
  (cond
   ((null n)
    (let ((size (- (buffer-line-length
                    (window-buffer)
                    (window-cur-linum))
                  (window-cur-col))))
      (if (zerop size)
        (delete-char 1)
        (delete-char size))))
   ((plusp n)
    (dotimes (_ n)
      (kill-line)))))

(defun goto-column (col)
  (setf (window-cur-col) col)
  (setf (window-max-col) col))

(define-key *global-keymap* "C-a" 'beginning-of-line)
(defcommand beginning-of-line () ()
  (goto-column 0)
  t)

(define-key *global-keymap* "C-e" 'end-of-line)
(defcommand end-of-line () ()
  (goto-column (buffer-line-length
                (window-buffer)
                (window-cur-linum)))
  t)

(define-key *global-keymap* "M-g" 'goto-line)
(defcommand goto-line (n) ("P")
  (unless n
    (setq n (read-number "nLine to GOTO: ")))
  (when (< 0 n (1+ (buffer-nlines (window-buffer))))
    (setf (window-cur-linum) n)
    t))

(define-key *global-keymap* "M-<" 'beginning-of-buffer)
(defcommand beginning-of-buffer () ()
  (goto-line 1)
  (goto-column 0)
  t)

(define-key *global-keymap* "M->" 'end-of-buffer)
(defcommand end-of-buffer () ()
  (goto-line (buffer-nlines (window-buffer)))
  (goto-column (buffer-line-length
                (window-buffer)
                (window-cur-linum)))
  t)

(defun %buffer-adjust-col (arg)
  (if arg
    (beginning-of-line)
    (setf (window-cur-col)
          (min (window-max-col)
               (buffer-line-length
                (window-buffer)
                (window-cur-linum))))))

(define-key *global-keymap* "C-n" 'next-line)
(defcommand next-line (&optional n) ("P")
  (if (and n (minusp n))
    (prev-line (- n))
    (if (dotimes (_ (or n 1) t)
          (if (tail-line-p *current-window* (window-cur-linum))
            (return nil)
            (incf (window-cur-linum))))
      (progn (%buffer-adjust-col n) t)
      (progn (end-of-line) nil))))

(define-key *global-keymap* "C-p" 'prev-line)
(defcommand prev-line (&optional n) ("P")
  (if (and n (minusp n))
    (next-line (- n))
    (if (dotimes (_ (or n 1) t)
          (if (head-line-p *current-window* (window-cur-linum))
            (return)
            (decf (window-cur-linum))))
      (progn (%buffer-adjust-col n) t)
      (progn (beginning-of-line) nil))))

(define-key *global-keymap* "C-f" 'next-char)
(defcommand next-char (&optional (n 1)) ("p")
  (if (minusp n)
    (prev-char (- n))
    (dotimes (_ n t)
      (cond
       ((eobp)
        (return nil))
       ((eolp)
        (next-line 1))
       (t
        (goto-column (1+ (window-cur-col))))))))

(define-key *global-keymap* "C-b" 'prev-char)
(defcommand prev-char (&optional (n 1)) ("p")
  (if (minusp n)
    (next-char (- n))
    (dotimes (_ n t)
      (cond
       ((bobp)
        (return nil))
       ((bolp)
        (prev-line 1)
        (end-of-line))
       (t
        (goto-column (1- (window-cur-col))))))))

(define-key *global-keymap* "C-v" 'next-page)
(defcommand next-page (&optional (n 1)) ("p")
  (scroll-down (* n (- (window-nlines) 1))))

(define-key *global-keymap* "M-v" 'prev-page)
(defcommand prev-page (&optional (n 1)) ("p")
  (scroll-up (* n (- (window-nlines) 1))))

(define-key *global-keymap* "C-@" 'mark-set)
(defcommand mark-set () ()
  (let ((buffer (window-buffer)))
    (setf (buffer-mark-linum buffer)
      (window-cur-linum))
    (setf (buffer-mark-col buffer)
      (window-cur-col))
    (write-message "Mark set")
    t))

(define-key *global-keymap* "C-xC-x" 'exchange-point-mark)
(defcommand exchange-point-mark () ()
  (let ((buffer (window-buffer)))
    (when (buffer-check-marked buffer)
      (psetf
       (window-cur-linum) (buffer-mark-linum buffer)
       (window-cur-col) (buffer-mark-col buffer)
       (buffer-mark-linum buffer) (window-cur-linum)
       (buffer-mark-col buffer) (window-cur-col))
      (setf (window-max-col) (buffer-mark-col buffer))
      t)))

(defun following-char ()
  (buffer-get-char (window-buffer)
    (window-cur-linum)
    (window-cur-col)))

(defun preceding-char ()
  (cond
   ((bobp)
    nil)
   ((bolp)
    (buffer-get-char (window-buffer)
      (1- (window-cur-linum))
      (buffer-line-length (window-buffer)
        (1- (window-cur-linum)))))
   (t
    (buffer-get-char (window-buffer)
      (window-cur-linum)
      (1- (window-cur-col))))))

(defun char-after (&optional (n 0))
  (let ((point (point)))
    (prog1 (when (next-char n)
             (prog1 (following-char)
                    (prev-char n)))
           (point-set point))))

(defun char-before (&optional (n 1))
  (let ((point (point)))
    (prog1 (when (prev-char (1- n))
             (prog1 (preceding-char)
                    (next-char (1- n))))
           (point-set point))))

(defun replace-char (c)
  (delete-char)
  (buffer-insert-char
   (window-buffer)
   (window-cur-linum)
   (window-cur-col)
   c))

(defun tab-line-aux (n make-space-str)
  (dotimes (_ n t)
    (let ((str (buffer-line-string (window-buffer) (window-cur-linum)))
          (count 0)
          index)
      (dotimes (i (length str))
        (setq index i)
        (case (aref str i)
          (#\space
           (incf count))
          (#\tab
           (setq count (char-width #\tab count)))
          (otherwise
           (return))))
      (multiple-value-bind (div mod) (floor count *tab-size*)
        (buffer-line-string-set (window-buffer) (window-cur-linum)
          (concatenate 'string
            (funcall make-space-str div)
            (make-string mod :initial-element #\space)
            (subseq str index)))))
    (unless (next-line 1)
      (return))))

(define-key *global-keymap* "C-xC-e" 'entab-line)
(defcommand entab-line (n) ("p")
  (tab-line-aux n
    (lambda (n)
      (make-string n :initial-element #\tab))))

(define-key *global-keymap* "C-xC-a" 'detab-line)
(defcommand detab-line (n) ("p")
  (tab-line-aux n
    (lambda (n)
      (make-string (* n *tab-size*) :initial-element #\space))))

(defun blank-line-p ()
  (let ((str (buffer-line-string
              (window-buffer)
              (window-cur-linum))))
    (dotimes (i (length str) (1+ (length str)))
      (let ((c (aref str i)))
        (unless (or (char= c #\space) (char= c #\tab))
          (return nil))))))

(define-key *global-keymap* "C-xC-o" 'delete-blank-lines)
(defcommand delete-blank-lines () ()
  (do ()
      ((not (blank-line-p))
       (next-line 1))
    (unless (prev-line 1)
      (return)))
  (do () ((eobp))
    (let ((result (blank-line-p)))
      (unless (and result (delete-char result))
        (return)))))

(define-key *global-keymap* "C-t" 'transpose-characters)
(defcommand transpose-characters () ()
  (cond
   ((bolp))
   ((eolp)
    (let* ((str (buffer-line-string (window-buffer) (window-cur-linum)))
           (len (length str)))
      (when (< 1 len)
        (buffer-line-string-set (window-buffer) (window-cur-linum)
          (concatenate 'string
            (subseq str 0 (- len 2))
            (string (aref str (- len 1)))
            (string (aref str (- len 2)))))))
    t)
   (t
    (let ((str (buffer-line-string (window-buffer) (window-cur-linum))))
      (buffer-line-string-set (window-buffer) (window-cur-linum)
        (concatenate 'string
          (subseq str 0 (- (window-cur-col) 1))
          (string (aref str (window-cur-col)))
          (string (aref str (1- (window-cur-col))))
          (subseq str (1+ (window-cur-col))))))
    t)))

(defcommand erase-buffer () ()
  (beginning-of-buffer)
  (buffer-erase (window-buffer))
  t)

(defcommand delete-while-whitespaces (&optional ignore-newline-p) ("P")
  (do ((n 0 (1+ n))) (nil)
    (let ((c (following-char)))
      (if (or (and ignore-newline-p (char= c #\newline))
              (not (syntax-space-char-p c)))
        (return n)
        (delete-char 1)))))

(defun insert-paren-hilighting-aux (c n)
  (when (insert-char c n)
    (let ((point (point)))
      (when (backward-list 1)
        (window-update-all)
        (cl-ncurses:timeout 1000)
        (let ((c (cl-ncurses:getch)))
          (unless (= -1 c) (cl-ncurses:ungetch c)))
        (cl-ncurses:timeout -1)
        (point-set point)
        t))))

(macrolet ((def (name c)
                `(progn
                   (define-key *global-keymap* ,(string c)
                               ',name)
                   (defcommand ,name (n) ("p")
                               (insert-paren-hilighting-aux ,c n)))))
  (def insert-paren-hilighting #\))
  (def insert-brace-hilighting #\])
  (def insert-block-hilighting #\}))
