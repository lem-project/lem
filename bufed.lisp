;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(unmark-buffer
          toggle-read-only
          rename-buffer
          bolp
          eolp
          bobp
          eobp
          insert-char
          quoted-insert
          insert-lines
          insert-string
          insert-newline
          newline-and-indent
          open-line
          delete-char
          backward-delete-char
          kill-line
          set-charpos
          beginning-of-line
          end-of-line
          goto-line
          beginning-of-buffer
          end-of-buffer
          forward-line
          next-line
          prev-line
          next-char
          prev-char
          next-page
          prev-page
          next-page-char
          prev-page-char
          mark-set
          exchange-point-mark
          following-char
          preceding-char
          char-after
          char-before
          replace-char
          entab-line
          detab-line
          blank-line-p
          delete-blank-lines
          transpose-characters
          erase-buffer
          delete-while-whitespaces
          skip-chars-forward
          skip-chars-backward
          just-one-space
          delete-indentation
          back-to-indentation
          indent-line
          undo
          redo))

(define-key *global-keymap* (kbd "M-~") 'unmark-buffer)
(define-command unmark-buffer () ()
  (setf (buffer-modified-p (current-buffer)) nil)
  t)

(define-key *global-keymap* (kbd "C-x C-q") 'toggle-read-only)
(define-command toggle-read-only () ()
  (setf (buffer-read-only-p (current-buffer))
        (not (buffer-read-only-p (current-buffer))))
  t)

(define-command rename-buffer (name) ("sRename buffer: ")
  (setf (buffer-name) name)
  t)

(defun head-line-p (linum)
  (<= linum 1))

(defun tail-line-p (buffer linum)
  (<= (buffer-nlines buffer) linum))

(defun bolp ()
  (zerop (window-current-charpos)))

(defun eolp ()
  (= (window-current-charpos)
     (buffer-line-length
      (current-buffer)
      (window-current-linum))))

(defun bobp ()
  (and (head-line-p (window-current-linum))
       (bolp)))

(defun eobp ()
  (and (tail-line-p
        (current-buffer)
        (window-current-linum))
       (eolp)))

(defun insert-char (c n)
  (dotimes (_ n t)
    (when (buffer-insert-char
           (current-buffer)
           (window-current-linum)
           (window-current-charpos)
           c)
      (next-char 1))))

(define-key *global-keymap* (kbd "C-q") 'quoted-insert)
(define-command quoted-insert (&optional (n 1)) ("p")
  (let ((c (getch)))
    (dotimes (_ n t)
      (cond ((char= c C-m)
             (insert-newline 1))
            ((char= c C-d)
             (delete-char))
            (t
             (insert-char c 1))))))

(defun insert-lines (lines)
  (do ((rest lines (cdr rest)))
      ((null rest))
    (buffer-insert-line
     (current-buffer)
     (window-current-linum)
     (window-current-charpos)
     (car rest))
    (next-char (length (car rest)))
    (when (cdr rest)
      (insert-newline 1))))

(define-command insert-string (str) ("sInsert string: ")
  (insert-lines (split-string str #\newline)))

(define-key *global-keymap* (kbd "C-m") 'insert-newline)
(define-command insert-newline (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (buffer-insert-newline (current-buffer)
                           (window-current-linum)
                           (window-current-charpos))
    (forward-line 1)))

(define-key *global-keymap* (kbd "C-j") 'newline-and-indent)
(define-command newline-and-indent (n) ("p")
  (dotimes (_ n t)
    (let ((spaces (second
                   (multiple-value-list
                    (count-indent)))))
      (unless (and (insert-newline 1)
                   (insert-string spaces))
        (return nil)))))

(define-key *global-keymap* (kbd "C-o") 'open-line)
(define-command open-line (n) ("p")
  (insert-newline n)
  (prev-char n))

(define-key *global-keymap* (kbd "C-d") 'delete-char)
(define-key *global-keymap* (kbd "[dc]") 'delete-char)
(define-command delete-char (&optional n does-not-kill-p) ("P")
  (cond
   ((and n (minusp n))
    (backward-delete-char (- n)))
   ((eobp) nil)
   (t
    (let ((lines
           (buffer-delete-char (current-buffer)
                               (window-current-linum)
                               (window-current-charpos)
                               (or n 1))))
      (when (and (not does-not-kill-p) n)
        (with-kill ()
          (kill-push lines)))
      t))))

(define-key *global-keymap* (kbd "C-h") 'backward-delete-char)
(define-key *global-keymap* (kbd "[backspace]") 'backward-delete-char)
(define-key *global-keymap* (kbd "[del]") 'backward-delete-char)
(define-command backward-delete-char (&optional n does-not-kill-p) ("P")
  (cond ((null n)
         (when (prev-char)
           (delete-char)))
        ((minusp n)
         (delete-char (- n) does-not-kill-p))
        (t
         (when (prev-char n)
           (delete-char n does-not-kill-p)))))

(define-key *global-keymap* (kbd "C-k") 'kill-line)
(define-command kill-line (&optional n) ("P")
  (cond
   ((null n)
    (delete-char
     (cond ((eolp) 1)
           ((blank-line-p))
           (t (- (buffer-line-length (current-buffer)
                                     (window-current-linum))
                 (window-current-charpos))))))
   ((plusp n)
    (dotimes (_ n)
      (kill-line)))))

(defun set-charpos (pos)
  (assert (<= 0 pos))
  (setf (window-current-charpos) pos))

(define-key *global-keymap* (kbd "C-a") 'beginning-of-line)
(define-key *global-keymap* (kbd "[home]") 'beginning-of-line)
(define-command beginning-of-line () ()
  (set-charpos 0)
  t)

(define-key *global-keymap* (kbd "C-e") 'end-of-line)
(define-key *global-keymap* (kbd "[end]") 'end-of-line)
(define-command end-of-line () ()
  (set-charpos (buffer-line-length
                (current-buffer)
                (window-current-linum)))
  t)

(define-key *global-keymap* (kbd "M-g") 'goto-line)
(define-command goto-line (n &optional does-not-recenter-p) ("P")
  (unless n
    (setq n (minibuf-read-number "Line to GOTO: ")))
  (when (< 0 n (1+ (buffer-nlines (current-buffer))))
    (setf (window-current-linum) n)
    (beginning-of-line)
    (unless does-not-recenter-p
      (recenter))
    t))

(define-key *global-keymap* (kbd "M-<") 'beginning-of-buffer)
(define-command beginning-of-buffer () ()
  (goto-line 1 t)
  (set-charpos 0)
  t)

(define-key *global-keymap* (kbd "M->") 'end-of-buffer)
(define-command end-of-buffer () ()
  (goto-line (buffer-nlines (current-buffer)) t)
  (set-charpos (buffer-line-length
                (current-buffer)
                (window-current-linum)))
  t)

(defun forward-line (&optional n)
  (unless n (setq n 1))
  (let ((dir (if (plusp n) 1 -1)))
    (prog1 (dotimes (_ (abs n) t)
             (when (if (plusp n)
                       (tail-line-p (current-buffer) (window-current-linum))
                       (head-line-p (window-current-linum)))
               (return nil))
             (incf (window-current-linum) dir))
      (beginning-of-line))))

(let ((tmp-column))
  (defun %next-line-before ()
    (when-interrupted-flag :next-line
      (setq tmp-column
            (str-width (buffer-line-string
                        (current-buffer)
                        (window-current-linum))
                       0
                       (window-current-charpos)))))
  (defun %next-line-after ()
    (let ((pos (or (wide-index (buffer-line-string
                                (current-buffer)
                                (window-current-linum))
                               tmp-column)
                   (buffer-line-length
                    (current-buffer)
                    (window-current-linum)))))
      (when pos
        (setf (window-current-charpos) pos)))
    (check-type (window-current-charpos)
                (integer 0 #.most-positive-fixnum))))

(define-key *global-keymap* (kbd "C-n") 'next-line)
(define-key *global-keymap* (kbd "[down]") 'next-line)
(define-command next-line (&optional n) ("p")
  (%next-line-before)
  (unless (prog1 (forward-line n)
            (%next-line-after))
    (cond ((plusp n)
           (end-of-buffer)
           (editor-error "End of buffer"))
          (t
           (beginning-of-buffer)
           (editor-error "Beginning of buffer"))))
  t)

(define-key *global-keymap* (kbd "C-p") 'prev-line)
(define-key *global-keymap* (kbd "[up]") 'prev-line)
(define-command prev-line (&optional n) ("p")
  (next-line (- n)))

(define-key *global-keymap* (kbd "C-f") 'next-char)
(define-key *global-keymap* (kbd "[right]") 'next-char)
(define-command next-char (&optional (n 1)) ("p")
  (if (minusp n)
      (prev-char (- n))
      (dotimes (_ n t)
        (cond
         ((eobp)
          (return nil))
         ((eolp)
          (forward-line 1))
         (t
          (set-charpos (1+ (window-current-charpos))))))))

(define-key *global-keymap* (kbd "C-b") 'prev-char)
(define-key *global-keymap* (kbd "[left]") 'prev-char)
(define-command prev-char (&optional (n 1)) ("p")
  (if (minusp n)
      (next-char (- n))
      (dotimes (_ n t)
        (cond
         ((bobp)
          (return nil))
         ((bolp)
          (forward-line -1)
          (end-of-line))
         (t
          (set-charpos (1- (window-current-charpos))))))))

(define-key *global-keymap* (kbd "C-v") 'next-page)
(define-key *global-keymap* (kbd "[npage]") 'next-page)
(define-command next-page (&optional n) ("P")
  (if n
      (scroll-down n)
      (let ((point (current-point)))
        (cond ((forward-line (1- (window-height)))
               (window-recenter (current-window))
               t)
              ((and (point-set point) nil))
              ((not (eobp))
               (end-of-buffer)
               (window-recenter (current-window))
               t)))))

(define-key *global-keymap* (kbd "M-v") 'prev-page)
(define-key *global-keymap* (kbd "[ppage]") 'prev-page)
(define-command prev-page (&optional n) ("P")
  (if n
      (scroll-up n)
      (let ((point (current-point)))
        (cond ((forward-line (- (1- (window-height))))
               (window-recenter (current-window))
               t)
              ((and (point-set point) nil))
              ((not (bobp))
               (beginning-of-buffer)
               (window-recenter (current-window))
               t)))))

(define-key *global-keymap* (kbd "C-x ]") 'next-page-char)
(define-command next-page-char (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (when (eq C-L (following-char))
      (next-char 1))
    (unless (search-forward (string #\page))
      (end-of-buffer)
      (return nil))))

(define-key *global-keymap* (kbd "C-x [") 'prev-page-char)
(define-command prev-page-char (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (when (eq C-L (preceding-char))
      (prev-char 1))
    (unless (search-backward (string #\page))
      (beginning-of-buffer)
      (return nil))))

(define-key *global-keymap* (kbd "C-@") 'mark-set)
(define-command mark-set () ()
  (let ((buffer (current-buffer)))
    (setf (buffer-mark-p buffer) t)
    (if (buffer-mark-marker)
        (setf (marker-point (buffer-mark-marker buffer))
              (current-point))
        (setf (buffer-mark-marker buffer)
              (make-marker-current-point)))
    (minibuf-print "Mark set")
    t))

(define-key *global-keymap* (kbd "C-x C-x") 'exchange-point-mark)
(define-command exchange-point-mark () ()
  (let ((buffer (current-buffer)))
    (when (buffer-check-marked buffer)
      (psetf
       (window-current-linum) (marker-linum (buffer-mark-marker buffer))
       (window-current-charpos) (marker-charpos (buffer-mark-marker buffer))
       (marker-linum (buffer-mark-marker buffer)) (window-current-linum)
       (marker-charpos (buffer-mark-marker buffer)) (window-current-charpos))
      (assert (<= 0 (window-current-charpos)))
      t)))

(defun following-char ()
  (buffer-get-char (current-buffer)
                   (window-current-linum)
                   (window-current-charpos)))

(defun preceding-char ()
  (cond
   ((bobp)
    nil)
   ((bolp)
    (buffer-get-char (current-buffer)
                     (1- (window-current-linum))
                     (buffer-line-length (current-buffer)
                                         (1- (window-current-linum)))))
   (t
    (buffer-get-char (current-buffer)
                     (window-current-linum)
                     (1- (window-current-charpos))))))

(defun char-after (&optional (n 0))
  (if (zerop n)
      (following-char)
      (let ((point (current-point)))
        (if (next-char n)
            (prog1 (following-char)
              (prev-char n))
            (progn
              (point-set point)
              nil)))))

(defun char-before (&optional (n 1))
  (if (= n 1)
      (preceding-char)
      (let ((point (current-point)))
        (if (prev-char (1- n))
            (prog1 (preceding-char)
              (next-char (1- n)))
            (progn
              (point-set point)
              nil)))))

(defun replace-char (c)
  (delete-char)
  (buffer-insert-char
   (current-buffer)
   (window-current-linum)
   (window-current-charpos)
   c))

(defun count-indent ()
  (save-excursion
   (beginning-of-line)
   (let ((count 0)
         (chars))
     (do () ((eolp))
       (let ((c (following-char)))
         (case c
           (#\space
            (incf count))
           (#\tab
            (setq count (char-width #\tab count)))
           (otherwise
            (return)))
         (push c chars)
         (next-char 1)))
     (values count
             (coerce (nreverse chars) 'string)))))

(defun tab-line-aux (n make-space-str)
  (dotimes (_ n t)
    (let ((count (count-indent)))
      (multiple-value-bind (div mod)
          (floor count *tab-size*)
        (beginning-of-line)
        (delete-while-whitespaces t nil)
        (insert-string (funcall make-space-str div))
        (insert-char #\space mod)))
    (unless (forward-line 1)
      (return))))

(define-command entab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string n :initial-element #\tab))))

(define-command detab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string (* n *tab-size*) :initial-element #\space))))

(defun blank-line-p ()
  (let ((string (buffer-line-string (current-buffer) (window-current-linum)))
        (eof-p (buffer-end-line-p (current-buffer) (window-current-linum))))
    (when (string= "" (string-trim '(#\space #\tab) string))
      (+ (length string)
         (if eof-p 0 1)))))

(define-key *global-keymap* (kbd "C-x C-o") 'delete-blank-lines)
(define-command delete-blank-lines () ()
  (do ()
      ((not (blank-line-p))
       (forward-line 1))
    (unless (forward-line -1)
      (return)))
  (do () ((eobp))
    (let ((result (blank-line-p)))
      (unless (and result (delete-char result t))
        (return)))))

(define-key *global-keymap* (kbd "C-t") 'transpose-characters)
(define-command transpose-characters () ()
  (cond ((bolp))
        ((eolp)
         (let* ((c1 (char-before 1))
                (c2 (char-before 2)))
           (unless (eql c2 #\newline)
             (backward-delete-char)
             (backward-delete-char)
             (insert-char c1 1)
             (insert-char c2 1))))
        (t
         (let* ((c1 (following-char))
                (c2 (preceding-char)))
           (delete-char)
           (backward-delete-char)
           (insert-char c1 1)
           (insert-char c2 1)))))

(define-command erase-buffer () ()
  (beginning-of-buffer)
  (buffer-erase (current-buffer))
  (beginning-of-buffer)
  t)

(defun delete-while-whitespaces (&optional ignore-newline-p use-kill-ring)
  (do ((n 0 (1+ n))) ((eobp))
    (let ((c (following-char)))
      (if (or (and ignore-newline-p (char= c #\newline))
              (not (syntax-space-char-p c)))
          (return n)
          (delete-char 1 (not use-kill-ring))))))

(macrolet ((def (name at-char step-char)
                `(defun ,name (pred &optional not-p)
                   (do ()
                       ((not (if (if (or (functionp pred)
                                         (symbolp pred))
                                     (funcall pred (,at-char))
                                     (find (,at-char) pred))
                                 (not not-p)
                                 not-p))
                        t)
                     (unless (,step-char)
                       (return))))))
  (def skip-chars-forward following-char next-char)
  (def skip-chars-backward preceding-char prev-char))

(define-key *global-keymap* (kbd "M-Spc") 'just-one-space)
(define-command just-one-space () ()
  (skip-chars-backward 'syntax-space-char-p)
  (delete-while-whitespaces nil nil)
  (insert-char #\space 1)
  t)

(define-key *global-keymap* (kbd "M-^") 'delete-indentation)
(define-command delete-indentation () ()
  (beginning-of-line)
  (let ((point (current-point)))
    (forward-line -1)
    (end-of-line)
    (delete-char (region-count (current-point) point) t)
    (just-one-space)
    t))

(define-key *global-keymap* (kbd "M-m") 'back-to-indentation)
(define-command back-to-indentation () ()
  (beginning-of-line)
  (skip-chars-forward #'(lambda (c) (member c '(#\space #\tab))))
  t)

(defun indent-line (column)
  (when (minusp column) (setq column 0))
  (let* ((old-column (current-column))
         (old-indent-string
          (save-excursion
           (region-string (progn (beginning-of-line) (current-point))
                          (progn (back-to-indentation) (current-point)))))
         (new-indent-string
          (if (get-bvar :indent-tabs-mode :default t)
              (multiple-value-bind (div mod)
                  (floor column *tab-size*)
                (concatenate 'string
                             (make-string div :initial-element #\tab)
                             (make-string mod :initial-element #\space)))
              (make-string column :initial-element #\space))))
    (cond ((string/= old-indent-string new-indent-string)
           (beginning-of-line)
           (delete-char (length old-indent-string) t)
           (insert-string new-indent-string)
           (if (< old-column column)
               (back-to-indentation)
               (move-to-column
                (max 0
                     (+ old-column
                        (- (str-width new-indent-string)
                           (str-width old-indent-string)))))))
          ((< old-column column)
           (back-to-indentation)))
    t))

(define-key *global-keymap* (kbd "C-\\") 'undo)
(define-command undo () ()
  (let ((point (buffer-undo (current-buffer))))
    (when point
      (point-set point)
      t)))

(define-key *global-keymap* (kbd "C-_") 'redo)
(define-command redo () ()
  (let ((point (buffer-redo (current-buffer))))
    (when point
      (point-set point)
      t)))
