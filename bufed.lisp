;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(bolp
          eolp
          bobp
          eobp
          insert-char
          insert-lines
          insert-string
          insert-newline
          delete-char
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
          undo
          redo))

(defun head-line-p ()
  (<= (current-linum) 1))

(defun tail-line-p ()
  (<= (buffer-nlines (current-buffer))
      (current-linum)))

(defun bolp ()
  (zerop (current-charpos)))

(defun eolp ()
  (= (current-charpos)
     (buffer-line-length
      (current-buffer)
      (current-linum))))

(defun bobp ()
  (and (head-line-p) (bolp)))

(defun eobp ()
  (and (tail-line-p) (eolp)))

(defun insert-char (c n)
  (dotimes (_ n t)
    (when (buffer-insert-char
           (current-buffer)
           (current-linum)
           (current-charpos)
           c)
      (next-char 1))))

(defun insert-lines (lines)
  (do ((rest lines (cdr rest)))
      ((null rest))
    (buffer-insert-line
     (current-buffer)
     (current-linum)
     (current-charpos)
     (car rest))
    (next-char (length (car rest)))
    (when (cdr rest)
      (insert-newline 1))))

(defun insert-string (str)
  (insert-lines (split-string str #\newline)))

(defun insert-newline (&optional (n 1))
  (dotimes (_ n)
    (buffer-insert-newline (current-buffer)
                           (current-linum)
                           (current-charpos)))
  (forward-line n))

(defun delete-char (n &optional killp)
  (when (minusp n)
    (setf n (- n))
    (unless (prev-char n)
      (return-from delete-char nil)))
  (if (eobp)
      nil
      (let ((lines
              (buffer-delete-char (current-buffer)
                                  (current-linum)
                                  (current-charpos)
                                  n)))
        (when killp
          (with-kill ()
            (kill-push lines)))
        t)))

(defun set-charpos (pos)
  (assert (<= 0
              pos
              (buffer-line-length (current-buffer) (current-linum))))
  (setf (current-charpos) pos))

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
                (current-linum)))
  t)

(define-key *global-keymap* (kbd "M-g") 'goto-line)
(define-command goto-line (n &optional does-not-recenter-p) ("P")
  (unless n
    (setq n (minibuf-read-number "Line to GOTO: ")))
  (when (< 0 n (1+ (buffer-nlines (current-buffer))))
    (setf (current-linum) n)
    (beginning-of-line)
    (unless does-not-recenter-p
      (recenter))
    t))

(define-key *global-keymap* (kbd "M-<") 'beginning-of-buffer)
(define-command beginning-of-buffer () ()
  (point-set (point-min))
  t)

(define-key *global-keymap* (kbd "M->") 'end-of-buffer)
(define-command end-of-buffer () ()
  (point-set (point-max))
  t)

(defun forward-line (&optional (n 1))
  (beginning-of-line)
  (if (plusp n)
      (dotimes (_ n t)
        (when (tail-line-p)
          (end-of-line)
          (return))
        (incf (current-linum)))
      (dotimes (_ (- n) t)
        (when (head-line-p)
          (return))
        (decf (current-linum)))))

(let ((tmp-column))
  (defun %next-line-before ()
    (when-interrupted-flag :next-line
                           (setq tmp-column
                                 (str-width (buffer-line-string
                                             (current-buffer)
                                             (current-linum))
                                            0
                                            (current-charpos)))))
  (defun %next-line-after ()
    (let ((pos (or (wide-index (buffer-line-string
                                (current-buffer)
                                (current-linum))
                               tmp-column)
                   (buffer-line-length
                    (current-buffer)
                    (current-linum)))))
      (when pos
        (setf (current-charpos) pos)))
    (check-type (current-charpos)
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
           (set-charpos (1+ (current-charpos))))))))

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
           (set-charpos (1- (current-charpos))))))))

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
    (buffer-check-marked buffer)
    (psetf
     (current-linum) (marker-linum (buffer-mark-marker buffer))
     (current-charpos) (marker-charpos (buffer-mark-marker buffer))
     (marker-linum (buffer-mark-marker buffer)) (current-linum)
     (marker-charpos (buffer-mark-marker buffer)) (current-charpos))
    (assert (<= 0 (current-charpos)))
    t))

(defun following-char ()
  (buffer-get-char (current-buffer)
                   (current-linum)
                   (current-charpos)))

(defun preceding-char ()
  (cond
    ((bobp)
     nil)
    ((bolp)
     (buffer-get-char (current-buffer)
                      (1- (current-linum))
                      (buffer-line-length (current-buffer)
                                          (1- (current-linum)))))
    (t
     (buffer-get-char (current-buffer)
                      (current-linum)
                      (1- (current-charpos))))))

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
  (delete-char 1 nil)
  (buffer-insert-char
   (current-buffer)
   (current-linum)
   (current-charpos)
   c))

(defun blank-line-p ()
  (let ((string (buffer-line-string (current-buffer) (current-linum)))
        (eof-p (buffer-end-line-p (current-buffer) (current-linum))))
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
      (unless (and result (delete-char result nil))
        (return)))))

(define-key *global-keymap* (kbd "C-t") 'transpose-characters)
(define-command transpose-characters () ()
  (cond ((bolp))
        ((eolp)
         (let* ((c1 (char-before 1))
                (c2 (char-before 2)))
           (unless (eql c2 #\newline)
             (delete-char -2 nil)
             (insert-char c1 1)
             (insert-char c2 1))))
        (t
         (let* ((c1 (following-char))
                (c2 (preceding-char)))
           (delete-char 1 nil)
           (delete-char -1 nil)
           (insert-char c1 1)
           (insert-char c2 1)))))

(define-command erase-buffer () ()
  (point-set (point-max))
  (buffer-erase (current-buffer))
  t)

(defun delete-while-whitespaces (&optional ignore-newline-p use-kill-ring)
  (do ((n 0 (1+ n))) ((eobp))
    (let ((c (following-char)))
      (if (or (and ignore-newline-p (char= c #\newline))
              (not (syntax-space-char-p c)))
          (return n)
          (delete-char 1 use-kill-ring)))))

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
    (delete-char (region-count (current-point) point) nil)
    (just-one-space)
    t))

(define-key *global-keymap* (kbd "M-m") 'back-to-indentation)
(define-command back-to-indentation () ()
  (beginning-of-line)
  (skip-chars-forward '(#\space #\tab))
  t)

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
